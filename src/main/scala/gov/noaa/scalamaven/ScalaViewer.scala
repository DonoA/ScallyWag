package gov.noaa.scalamaven

import java.io.BufferedReader
import javax.script.ScriptEngine
import javax.script.ScriptEngineManager
import scala.collection.mutable.HashMap

object ScalaViewer {
    def parseDocument(document: BufferedReader, scope: HashMap[String, Object]): StringBuilder = {
        
        val jsFactory = new ScriptEngineManager();
        val jsEngine = jsFactory.getEngineByName("JavaScript");
        
        for((key, value) <- scope){
            jsEngine.put(key, value)
        }
        
        val rtnBldr = new StringBuilder()
        var line = document.readLine
        var inTrueBlock = true
        while(line != null){
            val execStmt = locateExecBlock(line) // cannot handle many execs in one line
            val evalStmt = evalStatement(execStmt.code, jsEngine, inTrueBlock)
            inTrueBlock = evalStmt._3
            if(inTrueBlock){
                if(execStmt.output){
                    rtnBldr.append(execStmt.preCode + evalStmt._1 + execStmt.postCode)
                }else{
                    rtnBldr.append(execStmt.preCode + execStmt.postCode)
                }
            }
            line = document.readLine
        }
        return rtnBldr
    }
    
    def locateExecBlock(segment: String): ExecSegment = {
        val strt = segment.indexOf("<%")
        if(strt == -1){
            return ExecSegment(segment, "", "", false)
        }else{
            val end = segment.indexOf("%>")
            if(end == -1){
                // error somehow
                return null
            }
            val output = segment(strt + 2).equals('=')
            var tagSize = 2
            if(output){
                tagSize = 3
            }
            if(segment(strt + 2).equals('#')){
                return ExecSegment(segment.substring(0, strt), 
                                   "", 
                                   segment.substring(end + 2), 
                                   output)
            }else{
                return ExecSegment(segment.substring(0, strt), 
                                   segment.substring(strt + tagSize, end), 
                                   segment.substring(end + 2), 
                                   output)
            }
        }
    }
    
    case class ExecSegment(preCode: String, code: String, postCode: String, output: Boolean)
    
    type Environment = HashMap[String, Object]
    
    def evalStatement(statement: String, engine: ScriptEngine, execBlock: Boolean): (Object, Int, Boolean) = {
        var scopeDiff = 0
        var contExec = true
        if(execBlock){
            if(statement.contains("if")){
                val (strt, end) = (statement.indexOf("("), statement.indexOf(")"))
                if(evalIf(statement.substring(strt+1, end-1), engine)){
                    scopeDiff += 1
                    contExec = true
                }else{
                    contExec = false
                }
            }else if(statement.contains("}")){
                scopeDiff -= 1
                if(statement.contains("else")){
                    contExec = false
                }
            }else{
                val rtn = engine.eval(statement)
                if(rtn.isInstanceOf[String]){
                    return (rtn, 0, true)
                }
            }
        }else{
            if(statement.contains("}")){
                if(statement.contains("if")){
                    val (strt, end) = (statement.indexOf("("), statement.indexOf(")"))
                    if(evalIf(statement.substring(strt+1, end-1), engine)){
                        contExec = true
                    }else{
                        scopeDiff -= 1
                        contExec = false
                    }
                }else{
                    contExec = true
                }
                if(!statement.contains("else")){
                    scopeDiff -= 1
                }
            }else{
                contExec = false
            }
        }
        return ("", scopeDiff, contExec)
    }
    
    def evalIf(stmt: String, engine: ScriptEngine): Boolean = {
        val (result, scope, cont) = evalStatement("var __EJSIF__ = (" + 
                                                    stmt.replace(" ", "") + 
                                                    ")", engine, true)
        return engine.get("__EJSIF__").asInstanceOf[Boolean].equals(true)
    }
    
    
//    
//    def readVarName(statement: String): (String, Int) = {
//        var index = 0
//        var seg = ""
//        while(index < statement.length){
//            statement(index) match {
//                case ' ' => return (seg, index)
//                case '=' => return (seg, index-1)
//                case default => {
//                    seg += statement(index)
//                    index += 1
//                }
//            }
//        }
//        return (seg, index)
//    }
//    
//    def readValue(statement: String): (String, Int) = {
//        var index = 0
//        var seg = ""
//        var str = false
//        while(index < statement.length){
//            statement(index) match {
//                case '"' => {
//                    if(str){
//                        str = false
//                    }
//                }
//                case '=' => return (seg, index)
//                case default => {
//                    seg += statement(index)
//                    index += 1
//                }
//            }
//        }
//        return (seg, index)
//    }
    
//    
//    def parseStatement(statement: String, scope: Environment): (Object, Int) = {
//        var scopeLevel = 0
//        var seg = ""
//        var index = 0
//        var key = ""
//        var statement = ""
//        var parens = false
//        var ignoredStatement = false
//        var innerStatement = ""
//        while(index < statement.length){
//            if(!ignoredStatement || statement(index).equals("}")) {
//                statement(index) match {
//                    case '(' => {
//                        parens = true
//                        key = seg
//                        seg = ""
//                        index += 1
//                    }
//                    case ')' => {
//                        parens = false
//                        innerStatement = seg
//                        seg = ""
//                    }
//                    case default => {
//                            
//                    }
//                }
//            }
//        }
//        return ("", scopeLevel)
//    }
//    
//    def evalObject(tag: String, scope: Environment): Any = {
//        val strt = tag.indexOf(".")
//        if(strt == -1){
//            val rtn = scope(tag)
//            if(rtn.isInstanceOf[()])
//        }else{
//            val objName = tag.substring(0, strt)
//            val prstrt = objName.indexOf("(")
//            if(prstrt == -1){
//                return getValueByName(tag.substring(strt+1), scope(objName))
//            }else{
//                val prend = objName.indexOf(")")
//                if(prend == -1){
//                    // some sort of error
//                    return
//                }
//                
//            }
//        }
//    }
//    
//    def parseParams(prms: String, scope: Environment): Array[Any] = {
//        var seg = ""
//        var index = 0
//        val params = ArrayBuffer[Any]()
//        while(index < prms.length){
//            prms(index) match {
//                case ',' => {
//                    params += evalObject(seg, scope)
//                }
//                case default => {
//                    seg += prms(index)
//                }
//            }
//        }
//        return params.toArray
//    }
//    
//    def getValueByName(tag: String, obj: Object): Any = {
//        val strt = tag.indexOf(".")
//        val tgName = tag.substring(0, strt)
//        try {
//            val f = obj.getClass.getField(tgName)
//            if(strt == -1){
//                return f.get(obj)
//            }else{
//                return 
//            }
//        }
//        catch {
//            case e: NoSuchFieldException => { 
//                
//            }
//        }
//        
//        for(m <- obj.getClass.getMethods) {
//            println("method: " + m.getName)
//            println("takes: " + m.getParameterCount)
//
//        }
//    }
}
