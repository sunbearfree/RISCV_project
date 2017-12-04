import java.io.{BufferedOutputStream, FileOutputStream}
import java.nio.file.{Files, Paths}
import java.nio.{ByteBuffer, IntBuffer}
import java.net

import collection.mutable.HashMap

object processor_main {
  var pc: Int = 0                                                    //Program counter
  var programLoopBreak: Int = 0                                      //Break variable for program loading loop
  var loopBreak: Int = 0                                             //Break variable for main program loop

  var reg: Array[Int] = Array.fill[Int](32)(0)                       //Register array
  var output: Array[Byte] = Array.fill[Byte](128)(0)                 //Output byte array

  reg(1) = 0x7ffffff0 // Initial state in venus for some reason
  reg(2) = 0x10000000

  var mem: HashMap[Int,Int] = new HashMap[Int,Int]()                 //Memory array

  var progr: HashMap[Int,Int] = new HashMap[Int,Int]()               //program array

  def main(args: Array[String]) {
    println("Hello RISC-V World!\n")

    // Load bin file
    val programByteArray = Files.readAllBytes(Paths.get("loop.bin"))
    while (programLoopBreak < programByteArray.length) {
      val byteStr: Int =  ((programByteArray(programLoopBreak + 3) & 0xff) << 24) +
                          ((programByteArray(programLoopBreak + 2) & 0xff) << 16) +
                          ((programByteArray(programLoopBreak + 1) & 0xff) << 8)  +
                          (programByteArray(programLoopBreak) & 0xff)
      progr(pc) = byteStr
      pc = pc + 4                                                   //Save instructions in a 32bit integer
      programLoopBreak = programLoopBreak + 4
    }
    val programLength: Int = pc                                     //Save program length and set counter to 0 again
    pc = 0

    while (loopBreak == 0) {                                        //Program loop

      val instr: Int = progr(pc)                                    //Load instruction from program array
      val opcode: Int = instr & 0x7f
      val rd: Int = (instr >> 7) & 0x1f
      val func3: Int = (instr >> 12) & 0x7
      val func7: Int = (instr >> 25) & 0x7f
      val rs1: Int = (instr >> 15) & 0x1f
      val rs2: Int = (instr >> 20) & 0x1f

      var imm_I: Int = (instr >> 20) & 0xfff
      if((imm_I>>11)==1) imm_I = imm_I ^ 0xfffff000                 // Check if negative

      var imm_S: Int = ((((instr >> 25) & 0x7f) << 5) + ((instr >> 7) & 0x1F))& 0xfff
      if((imm_S>>11)==1) imm_S = imm_S ^ 0xfffff000                 // Check if negative

      var imm_SB: Int = ((((instr >> 31) & 0x1) << 12) + (((instr >> 7) & 0x1) << 11) + (((instr >> 25) & 0x3f) << 5) + (((instr >> 8) & 0xf) << 1)) & 0xfff
      if((imm_SB>>11)==1) imm_SB = imm_SB ^ 0xfffff000              // Check if negative

      var imm_U: Int = (instr >> 12) & 0xfffff
      if((imm_U>>11)==1) imm_U = imm_U ^ 0xfff00000                 // Check if negative

      var imm_UJ: Int = ((((instr >> 31) & 0x1) << 20) + (((instr >> 12) & 0xff) << 12) + (((instr >> 20) & 0x1) << 11) + (((instr >> 21) & 0x3ff) << 1)) & 0xfffff
      if((imm_UJ>>11)==1) imm_UJ = imm_UJ ^ 0xfff00000              // Check if negative


      opcode match {                                                //Match statement for instructions. It checks in the order: Opcode -> func3 -> func7
        case 0x13 => func3 match {
          case 0x0 => reg(rd) = reg(rs1) + imm_I                    //ADDI
          case 0x1 => reg(rd) = reg(rs1) << imm_I                   //SLLI
          case 0x2 => if (reg(rd) < imm_I) {                        //SLTI
            reg(rd) = 1
          } else {
            reg(rd) = 0
          }
          case 0x3 => if (reg(rd) < imm_I) {                        //SLTIU
            reg(rd) = 1
          } else {
            reg(rd)
          }
          case 0x4 => reg(rd) = reg(rs1) ^ imm_I                    //XORI
          case 0x5 => func7 match {
            case 0x0 => reg(rd) = reg(rs1) >>> imm_I                //SRLI
            case 0x20 => reg(rd) = reg(rs1) >> imm_I                //SRAI
            case _ => println("ERROR7" + func3)
          }
          case 0x6 => reg(rd) = reg(rs1) | imm_I                    //ORI
          case 0x7 => reg(rd) = reg(rs1) & imm_I                    //ANDI
          case _ => println("ERROR3" + func3)
        }
        case 0x17 => reg(rd) = pc + (imm_U << 12)                   //AUIPC
        case 0x33 => func3 match { //R
          case 0x0 => func7 match { //f7
            case 0x0 => reg(rd) = reg(rs1) + reg(rs2)               //ADD
            case 0x20 => reg(rd) = reg(rs1) - reg(rs2)              //SUB
            case 0x1 => reg(rd) = reg(rs1) * reg(rs2)               //MUL
            case _ => println("ERROR7" + func7)
          }
          case 0x1 => reg(rd) = reg(rs1) << reg(rs2)                //SLL
          case 0x2 => if (reg(rs1) < reg(rs2)) {                    //SLT
            reg(rd) = 1
          } else {
            reg(rd) = 0
          }
          case 0x3 => if (reg(rs1) < reg(rs2)) {                    //SLTU
            reg(rd) = 1
          } else {
            reg(rd) = 0
          }
          case 0x4 => func7 match {   //f7
            case 0x0 => reg (rd) = reg (rs1) ^ reg (rs2)            //XOR
            case 0x1 => reg (rd) = reg (rs1) / reg (rs2)            //DIV
            case _ => println("ERROR7" + func7)
          }
          case 0x5 => func7 match { //f7
            case 0x0 => reg(rd) = reg(rs1) >>> reg(rs2)             //SRL
            case 0x20 => reg(rd) = reg(rs1) >> reg(rs2)             //SRA
            case _ => println("ERROR7" + func7)
          }
          case 0x6 => func7 match {
            case 0x0 =>reg(rd) = reg(rs1) | reg(rs2)                //OR
            case 0x1 =>reg(rd) = reg(rs1) % reg(rs2)                //REM
            case _ => println("ERROR7" + func7)
          }
          case 0x7 => reg(rd) = reg(rs1) & reg(rs2)                 //AND
          case _ => println("ERROR3" + func3)
        } //R done
        case 0x37 => reg(rd) = imm_U << 12                          //LUI
        case 0x23 => func3 match {                                              // Store instructions
          case 0x0 => mem(reg(rs1) + imm_S) = reg(rs2) & 0xFF       //SB
          case 0x1 => mem(reg(rs1) + imm_S) = reg(rs2) & 0xFFFF     //SH
          case 0x2 => mem(reg(rs1) + imm_S) = reg(rs2)              //SW
          case _ => println("ERROR3" + func3)
        }
        case 0x3 => func3 match {                                               //Load instructions
          case 0x0 => reg(rd) = mem.getOrElse(reg(rs1) + imm_I, 0) & 0XFF     //LB
          case 0x1 => reg(rd) = mem.getOrElse(reg(rs1) + imm_I, 0) & 0XFFFF   //LH
          case 0x2 => reg(rd) = mem.getOrElse(reg(rs1) + imm_I, 0)            //LW
          case 0x4 => reg(rd) = mem.getOrElse(reg(rs1) + imm_I, 0) & 0XFF     //LBU
          case 0x5 => reg(rd) = mem.getOrElse(reg(rs1) + imm_I, 0) & 0XFFFF   //LHU
          case _ => println("ERROR3" + func3)
        }
        case 0x63 => func3 match {
          case 0x0 => if (reg(rs2) == reg(rs1)) {                   //BEQ
            pc = pc + imm_SB - 4
          }
          case 0x1 => if (reg(rs1) != reg(rs2)) {                   //BNE
            pc = pc + imm_SB - 4
          }
          case 0x4 => if (reg(rs1) < reg(rs2)) {                    //BLT
            pc = pc + imm_SB - 4
          }
          case 0x5 => if (reg(rs1) >= reg(rs2)) {                   //BGE
            pc = pc + imm_SB - 4
          }
          case 0x6 => if ((reg(rs1) & 0x7fffffff) < (reg(rs2) & 0x7fffffff)) {  //BLTU
            pc = pc + imm_SB - 4
          }
          case 0x7 => if ((reg(rs1) & 0x7fffffff) >= (reg(rs2) & 0x7fffffff)) { //BGEU
            pc = pc + imm_SB - 4
          }
          case _ => println("ERROR3" + func3)
        }
        case 0x67 => func3 match {
          case 0x0 =>                                               //JALR
            reg(rd) = pc + 4
            pc = (reg(rs1)+imm_I) - 4
          case _ => println("ERROR3" + func3)
        }
        case 0x6f =>                                                //JAL
          reg(rd) = pc + 4
          pc = pc + imm_UJ - 4
        case 0x73 =>  reg(10) match {                               //ECALL
          case 0x1 => print(reg(11)) //not implemented
          case 0x4 => print(reg(11)) //not implemented
          case 0x9 => print(reg(11)) //not implemented
          case 0xa =>
            pc = programLength
          case 0xb => print(reg(11)) //not implemented
          case 0x11 =>
            print(reg(11))
            pc = programLength
          case _ => println("ERROR ecall" + reg(10))
        }
        case _ => {                                                 //If Opcode does not match any of the above print error code
          println("Opcode " + opcode + " not yet implemented:")
          println("Opcode = " + opcode)
          println("rd = " + rd)
          println("func3 = " + func3)
          println("func7 = " + func7)
          println("rs1 = " + rs1)
          println("rs2 = " + rs2)
          println("imm_I = " + imm_I)
          println("imm_S = " + imm_S)
          println("imm_SB = " + imm_SB)
          println("imm_U = " + imm_U)
          println("imm_UJ = " + imm_UJ + "\n")
        }
      }

      reg(0) = 0
      pc = pc + 4                                                   //Add to program counter

      if (pc >= programLength) {
        loopBreak = 1
      }
    }

    println("Register overview (decimal):")
    for (i <- reg.indices) {
      print(reg(i) + " ")

      output(i*1) = ((reg(i) >> 24) & 0xff).toByte
      output(i*2) = ((reg(i) >> 16) & 0xff).toByte
      output(i*3) = ((reg(i) >> 8) & 0xff).toByte
      output(i*3) = (reg(i) & 0xff).toByte
    }
    println("\n")

    println("Register overview (hex):")
    for (i <- 10 to 17) {
      print("%02d".format(i)+": ")
      (1 to (8-reg(i).toHexString.length())).foreach(_ => print("0"))
      print(reg(i).toHexString + "\n")
    }

    //register output to output.res
    val bos = new BufferedOutputStream(new FileOutputStream("output.res"))
    bos.write(output)
    bos.close()

    println("Program exit")
  }
}
