package io.dallen.scallywag

import java.lang.reflect.{Field, Method}
import java.sql.{Connection, Date, DriverManager, ResultSet}

import io.dallen.scallywag.Relations.BelongsTo
import io.dallen.scallywag.ScalaSequel.SequelData

import scala.annotation.meta.field
import scala.annotation.StaticAnnotation
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._

object ScalaSequel {

    case class FieldInfo(annoInfo: mutable.HashMap[String, String], fieldClass: Class[_],
                         relationInfo: Option[(TermSymbol, Class[_])])

    var conn: Connection = _

    val schemaByDataClassName = new mutable.HashMap[String, SequelType[_]]()

    val relationMapping = new mutable.HashMap[(Class[_], Class[_]), (Method, Method)]()

    case class ColumnDef(var name: String, var value: String)

    var dropAll = false

    def establishConnection(user: String, pass: String, db: String, port: Int = 3306,
                            host: String = "localhost", drop: Boolean = false): Unit = {
        Class.forName("com.mysql.jdbc.Driver")
        conn = DriverManager.getConnection(s"jdbc:mysql://$host:$port/$db?autoReconnect=true&useSSL=false",user,pass)
        dropAll = drop
    }

    abstract class SequelType[T <: SequelData](table: String) {

        var idCol = "id"

        var dataClass: Class[T] = _

        val fields = new mutable.HashMap[String, FieldInfo]()

        val columns = new mutable.HashMap[String, ColumnDef]()

        def getTable = table

        def find(id: Int): T = {
            val stmt = conn.createStatement()
            val rs = stmt.executeQuery(s"SELECT * FROM $table WHERE $idCol = $id")
            rs.next()
            val ninst = instanciateNull(dataClass)
            fields.foreach(entry => {
                val (name, fieldInfo) = entry
                fieldInfo.fieldClass match {
                    case c if c == classOf[Relations.HasOne[_]] && fieldInfo.relationInfo.isDefined => {
                        val foreignSchem = schemaByDataClassName(fieldInfo.relationInfo.get._2.getName)
                        val forgienColumn = ninst.getClass.getSimpleName+"_id"
                        val selectStatment = s"SELECT ${foreignSchem.idCol} " +
                            s"FROM ${foreignSchem.getTable} " +
                            s"WHERE $forgienColumn = ${rs.getInt(idCol)}"
                        println("Exec: " + selectStatment)
                        val foreignObject = stmt.executeQuery(selectStatment)
                        foreignObject.next
                        val other = instanciateNull(foreignSchem.dataClass).asInstanceOf[SequelData]
                        other.id = Some(foreignObject.getInt(foreignSchem.idCol))
                        dataClass.getMethod(name).invoke(ninst).asInstanceOf[Relations.HasOne[_]].data = Some(other)
                    }
                    case _ => locateSetter(dataClass, name, fieldInfo.fieldClass).get.invoke(ninst, rs.getObject(name))
                }
            })
            return ninst
        }

        def where(where: String): ResultSet = {
            val rs = conn.createStatement().executeQuery(s"SELECT * FROM $table WHERE $where")
            return rs
        }

        def insert(dt: SequelData): Unit = {
            val data = dt.asInstanceOf[T]
            val toInsert = mutable.HashMap[String, String]()
            columns.foreach(e => {
                val (fname, col) = e
                val f = dataClass.getMethod(fname).invoke(data)
                toInsert(col.name) = (f match {
                    case _: String => "\"" + f + "\""
                    case bt: Relations.BelongsTo[_] => bt.get().id.get
                    case a => a
                }).toString
            })
            toInsert("date_created") = "NOW()"
            toInsert("date_updated") = "NOW()"
            val strBldr = new mutable.StringBuilder()
            strBldr.append(s"insert into $table (")
            strBldr.append(toInsert.keySet.mkString(", "))
            strBldr.append(") values (")
            strBldr.append(toInsert.values.mkString(", "))
            strBldr.append(")")
            println(s"Exec: ${strBldr.mkString}")
            conn.createStatement().execute(strBldr.mkString)
            val rs = conn.createStatement().executeQuery("SELECT LAST_INSERT_ID();")
            rs.next
            dt.id = Some(rs.getInt(1))
            fields.foreach(e => {
                val (fname, t) = e
                val f = dataClass.getMethod(fname).invoke(data)
                f match {
                    case bt: Relations.HasOne[_] => {
                        if(bt.get.id.isEmpty){
                            bt.get.create()
                        }
                    }
                    case bt: Relations.HasMany[_] => {
                        bt.get().foreach(d => {
                            if(d.id.isEmpty){
                                d.create()
                            }
                        })
                    }
                    case _ =>
                }
            })
        }
    }

    abstract class SequelData {

        var id: Option[Int] = None

        var date_created: Option[Date] = None
        var date_updated: Option[Date] = None

//        def destroy(): Unit = {
//
//        }
//
        def create(): Unit = {
            schemaByDataClassName(this.getClass.getName).insert(this)
        }
    }

    def registerSequelType[T <: SequelData](typeScheme: SequelType[T], dataClass: Class[T]): Unit = {
        if(dropAll) {
            val drop = s"DROP TABLE ${typeScheme.getTable}"
            println(s"Exec: $drop")
            conn.createStatement().execute(drop)
        }
        typeScheme.dataClass = dataClass
        schemaByDataClassName(dataClass.getName) = typeScheme
        val tblCreateStr = new mutable.StringBuilder()
        tblCreateStr.append(s"create table ${typeScheme.getTable}(")
        val tableFields = new ArrayBuffer[String]()
        var foundAny = false
        tableFields += s"${typeScheme.idCol} int NOT NULL AUTO_INCREMENT"
        searchSequelFields(dataClass, (m, anno, annoInfo, relationClass) => {
            foundAny = true
            val col = ColumnDef(m.name.toString.replace(" ",""), "")
            var shouldInsert = true
            var foreignRelation: Option[(TermSymbol, Class[_])] = None
            relationClass match {
                case c if c == classOf[Relations.HasOne[_]] => {
                    if (!m.isVal) {
                        throw new UnsupportedOperationException(s"${m.name.toString} must be val")
                    }
                    shouldInsert = false
                    val foreignClass = runtimeMirror(c.getClassLoader).runtimeClass(m.typeSignature.typeArgs.head)
                    if(annoInfo.contains(SqlOps.foreignKey)){
                        searchSequelFields(foreignClass, (nm, nanno, nannoInfo, inRtClss) => {
                            if(nm.typeSignature.typeArgs.nonEmpty) {
                                val belongsType = runtimeMirror(inRtClss.getClassLoader)
                                    .runtimeClass(nm.typeSignature.typeArgs.head)
                                if (belongsType == dataClass && nannoInfo.contains(SqlOps.foreignKey) &&
                                    nannoInfo(SqlOps.foreignKey).equals(annoInfo(SqlOps.foreignKey))) {
                                    inRtClss match {
                                        case nc if nc == classOf[Relations.BelongsTo[_]] => {

                                        }
                                        case nc if nc == classOf[Relations.BelongsToMany[_, _]] => {

                                        }
                                    }
                                }
                            }
                        })
                    }else{
                        searchSequelFields(foreignClass, (nm, nanno, nannoInfo, foreignRelationType) => {
                            if(nm.typeSignature.typeArgs.nonEmpty) {
                                val belongsType = runtimeMirror(foreignRelationType.getClassLoader)
                                    .runtimeClass(nm.typeSignature.typeArgs.head)
                                if (belongsType == dataClass) {
                                    foreignRelationType match {
                                        case nc if nc == classOf[Relations.BelongsTo[_]] ||
                                            nc == classOf[Relations.BelongsToMany[_, _]] => {
                                            if (foreignRelation.isDefined) {
                                                throw new UnsupportedOperationException(
                                                    s"Many possible belongs for ${col.name}")
                                            }
                                            foreignRelation = Some((nm, foreignClass))
                                        }
                                        case _ => {}
                                    }
                                }
                            }
                        })
                    }
                    if(foreignRelation.isDefined){
                        relationMapping((dataClass, foreignClass)) =
                            (dataClass.getMethod(col.name),
                                foreignClass.getMethod(foreignRelation.get._1.name.toString.replace(" ", "")))
                    }
                }
                case c if c == classOf[Relations.HasMany[_]] => {
                    if (!m.isVal) {
                        throw new UnsupportedOperationException(s"${m.name.toString} must be val")
                    }
                    shouldInsert = false
                    val rtclss = runtimeMirror(c.getClassLoader).runtimeClass(m.typeSignature.typeArgs.head)

                    println("Detected many relation")
                }
                case c if c == classOf[Relations.BelongsTo[_]] => {
                    if (!m.isVal) {
                        throw new UnsupportedOperationException(s"${m.name.toString} must be val")
                    }
                    val rtclss = runtimeMirror(c.getClassLoader).runtimeClass(m.typeSignature.typeArgs.head)
                    col.name = rtclss.getSimpleName + "_id"
                    col.value = "int"
                }
                case c if c == classOf[Relations.BelongsToMany[_, _]] => {
                    if (!m.isVal) {
                        throw new UnsupportedOperationException(s"${m.name.toString} must be val")
                    }
                    shouldInsert = false
                    val rtclss = runtimeMirror(c.getClassLoader).runtimeClass(m.typeSignature.typeArgs.head)
                    val throughclss =
                        runtimeMirror(c.getClassLoader).runtimeClass(m.typeSignature.typeArgs.last)
                    col.name = throughclss.getSimpleName + "_id"
                    col.value = "int"
                    println("Detected belong many relation")
                }
                case c if c == classOf[String] && m.isVar => col.value = "varchar(40)"
                case c if c == classOf[Int] && m.isVar => col.value = "int"
                case typ => throw new UnsupportedOperationException(
                    s"Could not find sql type for ${m.name.toString} => $typ")
            }
            if(annoInfo.contains("column")){
                col.name = annoInfo("column")
            }
            if(annoInfo.contains("sqlType")){
                col.value = annoInfo("sqlType")
            }
            typeScheme.fields(m.name.toString.replace(" ", "")) = FieldInfo(annoInfo, relationClass, foreignRelation)
            if(shouldInsert) {
                typeScheme.columns(m.name.toString.replace(" ", "")) = col
                tableFields += s"${col.name} ${col.value}"
            }
        })
        tableFields ++= List("date_created DATETIME", "date_updated DATETIME", s"PRIMARY KEY (${typeScheme.idCol})")
        if(!foundAny){
            println(s"Warning: no fields found for class ${dataClass.getName}")
        }
        tblCreateStr.append(tableFields.mkString(", ") + ")")
        println(s"Exec: ${tblCreateStr.mkString}")

        try {
            conn.createStatement().execute(tblCreateStr.mkString)
        }catch{
            case _: Exception => println("Table already exists")
        }
    }

    def searchSequelFields(clz: Class[_], fn: (TermSymbol, Annotation, mutable.HashMap[String, String], Class[_]) => Unit): Unit = {
        val dataClassMirror: universe.Mirror = runtimeMirror(clz.getClassLoader)
        val clazz = dataClassMirror.classSymbol(clz)
        clazz.toType.members.collect {
            case m: TermSymbol => {
                val anno = m.annotations.find(sf => sf.tree.tpe.equals(typeOf[SequelField]))
                if (anno.isDefined) {
                    val runtimeClass: Class[_] =
                        dataClassMirror.runtimeClass(m.typeSignature.typeSymbol.asClass)
                    val fieldMap: mutable.HashMap[String, String] = parseAnnotationArgs(anno.get)
                    fn(m, anno.get, fieldMap, runtimeClass)
                }
            }
        }
    }

    def instanciateNull[T](clazz: Class[T]): T = {
        val defConstructor = clazz.getConstructors.head
        var nullParams = Seq[Object]()
        for(_ <- 1 to defConstructor.getParameterCount){
            nullParams = nullParams :+ null
        }
        return defConstructor.newInstance(nullParams:_*).asInstanceOf[T]
    }

    private def parseAnnotationArgs(anno: Annotation): mutable.HashMap[String, String] = {
        val fieldMap = mutable.HashMap[String, String]()
        if(anno.tree.children.nonEmpty) {
            anno.tree.children.tail.collect {
                case app: Apply => {
                    val key = app.args.head.collect({
                        case lit: Literal => lit.value.value.toString
                        case sel: Select =>
                            throw new UnsupportedOperationException(s"Non literal tags not supported: ${sel.toString()}")
                    }).head
                    val `val` = app.args.last.collect({
                        case lit: Literal => lit.value.value.toString
                        case sel: Select =>
                            throw new UnsupportedOperationException(s"Non literal tags not supported: ${sel.toString()}")
                    }).head
                    fieldMap(key) = `val`
                }
            }
        }
        return fieldMap
    }

    def locateSetter(cls: Class[_], fieldName: String, returnClass: Class[_]): Option[Method] = {
        try {
            return Some(cls.getMethod(fieldName + "_$eq", returnClass))
        }catch{
            case _: java.lang.NoSuchMethodException => {
                try {
                    return Some(cls.getMethod(fieldName + "_$eq", getNativeClass(returnClass.getName)))
                }catch {
                    case _: java.lang.NoSuchMethodException => {
                        return None
                    }
                }
            }
        }
        return None
    }

    private def getNativeClass(className: String): Class[_] = className match {
        case "scala.Int" => Integer.TYPE
        case _ => println(className); null
    }
}

object Relations {

    trait Has

    trait Belongs

    case class HasOne[T <: SequelData](caller: SequelData) extends Has {

        var data: Option[SequelData] = None

        def isLoaded(): Boolean = {
            return false
        }

        def get: T = data.get.asInstanceOf[T]

        def set(dt: T): Unit = {
            data = Some(dt)
            if(ScalaSequel.relationMapping.contains((caller.getClass, dt.getClass))){
                ScalaSequel.relationMapping(caller.getClass, dt.getClass)._2.invoke(dt) match {
                    case bt: BelongsTo[_] => bt.data = caller
                    case btm: BelongsToMany[_,_] => btm.data += caller
                }
            }
        }

        def :=(ndat: T): Unit = set(ndat)
    }

    case class HasMany[T <: SequelData](caller: SequelData) extends Has {

        var data = new mutable.ArrayBuffer[SequelData]()

        def this(caller: SequelData, data: T*) {
            this(caller)
            this.data ++= data
        }

        def isLoaded(): Boolean = {
            return false
        }

        def get(): mutable.ArrayBuffer[T] = data.asInstanceOf[mutable.ArrayBuffer[T]]

    }

    case class BelongsTo[T <: SequelData](caller: SequelData) extends Belongs {

        var data: SequelData = _

        def isLoaded(): Boolean = {
            return false
        }

        def get(): T = data.asInstanceOf[T]

    }

    case class BelongsToMany[T <: SequelData, U <: SequelData](caller: SequelData) extends Belongs {

        val data = mutable.ArrayBuffer[SequelData]()

        def isLoaded(): Boolean = {
            return false
        }

        def get(): mutable.ArrayBuffer[T] = data.asInstanceOf[mutable.ArrayBuffer[T]]
    }
}

object SqlOps {
    val sqlType = "sqlType"
    val foreignKey = "foreignKey"
    val column = "column"
    val through = "through"
}

@field
case class SequelField(options: (String, String)*) extends StaticAnnotation
