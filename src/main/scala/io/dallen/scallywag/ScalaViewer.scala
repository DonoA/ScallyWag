package io.dallen.scallywag

import java.io.BufferedReader
import java.util.Stack
import javax.script.ScriptEngine
import javax.script.ScriptEngineManager

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer

trait ViewRenderer {
    def parseDocument(document: BufferedReader, scope: immutable.HashMap[String, Object]): StringBuilder
}

object ScalaEjsViewer extends ViewRenderer {
    override def parseDocument(document: BufferedReader, scope: immutable.HashMap[String, Object]): StringBuilder = {
        
        val jsFactory = new ScriptEngineManager()
        val jsEngine = jsFactory.getEngineByName("JavaScript")
        
        for((key, value) <- scope){
            jsEngine.put(key, value)
        }
        
        val rtnBldr = new StringBuilder()
        var line = document.readLine
        val executionBlocks = new Stack[ExecutionBlock]()
        while(line != null){
            evalLine(line, rtnBldr, jsEngine, executionBlocks, true)
            line = document.readLine
        }
        return rtnBldr
    }
    
    def evalLine(line: String, rtnBldr: StringBuilder, jsEngine: ScriptEngine,
                 executionBlocks: Stack[ExecutionBlock], collectStack: Boolean) {
        var addLoop = true
        var execStmt = locateExecBlock(line)
        if(collectStack && !executionBlocks.empty()){
            executionBlocks.peek() match {
                case p: WhileBlock => p.loop += line
                case p: ForBlock => p.loop += line
                case _ => {}
            }
        }
        while(!execStmt.postCode.equals("") || addLoop){
            if(executionBlocks.empty || executionBlocks.peek.passedInitEval){
                rtnBldr.append(execStmt.preCode)
            }
            val evalStmt = evalStatement(execStmt.code, jsEngine, executionBlocks)
            if(execStmt.output){
                rtnBldr.append(evalStmt)
            }
            if(execStmt.postCode.equals("")){
                addLoop = false
            }else{
                execStmt = locateExecBlock(execStmt.postCode)
            }
        }
        rtnBldr.append("\n")
    }
    
    def locateExecBlock(segment: String): ExecSegment = {
//        println("Eval " + segment)
        val strt = segment.indexOf("<%")
        if(strt == -1){
            return ExecSegment(segment, "", "", false)
        }else{
            val end = segment.indexOf("%>")
            if(end == -1){
                println("Missing end tag!")
                // error somehow
                return null
            }
            var tagSize = 2
            if(segment(strt + 2).equals('=')){
                tagSize = 3
            }
            if(segment(strt + 2).equals('#')){
                return ExecSegment(segment.substring(0, strt), 
                                   "", 
                                   segment.substring(end + 2),
                                   false)
            }else{
                return ExecSegment(segment.substring(0, strt), 
                                   segment.substring(strt + tagSize, end), 
                                   segment.substring(end + 2),
                                   true)
            }
        }
    }
    
    case class ExecSegment(preCode: String, code: String, postCode: String, output: Boolean)
    
    abstract class ExecutionBlock(condition: String, var initEval: Boolean){
        def passedInitEval = initEval

        def setPassedInitEval(b: Boolean) {initEval = b}
    }

    case class IfBlock(var condition: String, var eval: Boolean) extends ExecutionBlock(condition, eval)

    case class WhileBlock(var condition: String, var loop: ArrayBuffer[String], var eval: Boolean) extends ExecutionBlock(condition, eval)

    case class ForBlock(var init: String, var condition: String, var incriment: String, var loop: ArrayBuffer[String],
        var eval: Boolean) extends ExecutionBlock(condition, eval)

    case class NoneBlock() extends ExecutionBlock("", false)

    def evalStatement(statement: String, engine: ScriptEngine, execStack: Stack[ExecutionBlock]): String = {
        var index = 0
        var seq = ""
        var parenDepth = 0
        var defBlock: ExecutionBlock = null
        val strBldr = new StringBuilder()
        var capture = false
        while(index < statement.length){
            if(!execStack.empty && !execStack.peek.passedInitEval){
                statement(index) match {
                    case '}' => {
                        execStack.peek match {
                            case WhileBlock(_,_,_) => execStack.pop
                            case ForBlock(_,_,_,_,_) => execStack.pop
                            case NoneBlock() => execStack.pop
                            case _ =>
                                    capture = true

                        }
                    }
                    case '{' => {
                        if(capture && handleElse(seq, execStack, engine)){
                            seq = ""
                            capture = false
                        }else{
                            execStack.push(NoneBlock())
                        }
                    }
                    case default => {
                        if(capture){
                            seq += statement(index)
                        }
                    }
                }
            }else{
                statement(index) match {
                    case '(' => {
                        parenDepth += 1
                        seq.replace(" ", "") match {
                            case "if" => {
                                defBlock = IfBlock(null, false)
                                seq = ""
                            }
                            case "for" => {
                                defBlock = ForBlock(null,null,null, new ArrayBuffer[String](), false)
                                seq = ""
                            }
                            case "while" => {
                                defBlock = WhileBlock(null, new ArrayBuffer[String](), false)
                                seq = ""
                            }
                            case defualt => {
                                seq += statement(index)
                            }
                        }
                    }
                    case ')' => {
                        parenDepth -= 1
                        if(parenDepth == 0){
                            defBlock match {
                                case ifblk: IfBlock => {
                                    ifblk.condition = seq
                                    ifblk.eval = evalCondition(seq, engine)
                                    defBlock.setPassedInitEval(ifblk.eval)
                                }
                                case whle: WhileBlock => {
                                    whle.condition = seq
                                    whle.eval = evalCondition(seq, engine)
                                    defBlock.setPassedInitEval(whle.eval)

                                }
                                case frblk: ForBlock => {
                                    frblk.incriment = seq
                                    engine.eval(frblk.init)
                                    frblk.eval = evalCondition(frblk.condition, engine)
                                    defBlock.setPassedInitEval(frblk.eval)
                                }
                            }
                            seq = ""
                        }else{
                            seq += statement(index)
                        }
                    }
                    case ';' => {
                        if(parenDepth == 1 && defBlock.isInstanceOf[ForBlock]){
                            val blk = defBlock.asInstanceOf[ForBlock]
                            if(blk.init == null){
                                blk.init = seq
                                seq = ""
                            }else{
                                blk.condition = seq
                                seq = ""
                            }
                        }else{
                            val rtn = engine.eval(seq)
                            if(rtn.isInstanceOf[String]){
                                strBldr.append(rtn.toString)
                            }
                            seq = ""
                        }
                    }
                    case '{' => {
                        if(defBlock != null){
                            execStack.push(defBlock)
                            defBlock = null
                            seq = ""
                        }else{
                            if(handleElse(seq, execStack, engine)){
                                capture = false
                                seq = ""
                            }else{
                                println("No Else!")
                            }
                        }
                    }
                    case '}' => {
                        execStack.peek match {
                            case p: WhileBlock => p.loop.remove(p.loop.size - 1)
                            case p: ForBlock => p.loop.remove(p.loop.size - 1)
                            case _ => {}
                        }
                        val (output, nCap) = completeBlock(execStack.peek, engine, execStack)
                        strBldr.append(output)
                        capture = nCap
                    }
                    case _ => {
                            seq += statement(index)
                    }
                }
            }
            index += 1
        }
        if(capture){
            execStack.pop
        }
        if(parenDepth != 0){
            println("parenDepth != 0")
            return null
        }
        val rtn = engine.eval(seq)
        if(rtn.isInstanceOf[String]){
            strBldr.append(rtn.toString)
        }
        return strBldr.mkString
    }
    
    def handleElse(seq: String, execStack: Stack[ExecutionBlock], engine: ScriptEngine): Boolean = {
        if(seq.contains("else")){
            if(seq.contains("if")){
                val (strt, end) = (seq.indexOf("("), seq.indexOf(")"))
                execStack.peek.asInstanceOf[IfBlock].condition = seq.substring(strt+1, end-1)
                execStack.peek.asInstanceOf[IfBlock].eval =
                    evalCondition(execStack.peek.asInstanceOf[IfBlock].condition, engine)
            }else{
                execStack.peek.asInstanceOf[IfBlock].condition =
                    "!(" + execStack.peek.asInstanceOf[IfBlock].condition + ")"
                execStack.peek.asInstanceOf[IfBlock].eval = !execStack.peek.asInstanceOf[IfBlock].eval
                execStack.peek.setPassedInitEval(execStack.peek.asInstanceOf[IfBlock].eval)
            }
            return true
        }else{
            return false
        }
    }
    
    def completeBlock(block: ExecutionBlock, engine: ScriptEngine, execStack: Stack[ExecutionBlock]): (String, Boolean) = block match {
        case IfBlock(_, _) => return ("", true)
        case ForBlock(init, condition, increment, lines, _) => {
            engine.eval(increment)
            val strBldr = new StringBuilder()
            while(evalCondition(condition, engine)){
                for(line <- lines){
                    evalLine(line, strBldr, engine, execStack, false)
                }
                engine.eval(increment)
            }
            execStack.pop
            return (strBldr.mkString, false)
        }
        case WhileBlock(condition, lines, _) => {
            val strBldr = new StringBuilder()
            while(evalCondition(condition, engine)){
                for(line <- lines){
                    evalLine(line, strBldr, engine, execStack, false)
                }
                println(condition + " is " + evalCondition(condition, engine))
            }
            execStack.pop
            return (strBldr.mkString, false)
        }
    }
    
    def evalCondition(cond: String, engine: ScriptEngine): Boolean = {
        val result = engine.eval("var __EJSIF__ = (" + cond + ")")
        return engine.get("__EJSIF__").asInstanceOf[Boolean].equals(true)
    }
}
