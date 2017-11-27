import java.nio.file.{Files, Paths}
import collection.mutable.HashMap

object processor_main {
  var pc: Int = 0                                                    //Program counter
  var programLoopBreak: Int = 0                                      //Break variable for program loading loop
  var loopBreak: Int = 0                                             //Break variable for main program loop
  var computenumber: Int = 1

  var reg: Array[Int] = Array.fill[Int](32)(0)                       //Register array

  reg(1) = 0x7ffffff0 // Initial state in venus for some reason
  reg(2) = 0x10000000

  var mem: HashMap[Int,Int] = new HashMap[Int,Int]()                 //Memory array - kunne det klares med en Byte????

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

    println("Program length: "+programLength/4)

    //for (i <- 0 to programLength/4-1) {
    //  (1 to (8-progr(i*4).toHexString.length())).foreach(_ => print("0"))
    //  println(progr(i*4).toHexString)
    //}

    while (loopBreak == 0) {                                        //Program loop

      print("%02d".format(computenumber)+": ")
      computenumber = computenumber + 1
      (1 to (8-progr(pc).toHexString.length())).foreach(_ => print("0"))
      print(progr(pc).toHexString)
      print(" pc: "+pc/4)
      println()

      val instr: Int = progr(pc)                                    //Load instruction from program array
      val opcode: Int = instr & 0x7f
      val rd: Int = (instr >> 7) & 0x1f
      val func3: Int = (instr >> 12) & 0x7
      val func7: Int = (instr >> 25) & 0x7f
      val rs1: Int = (instr >> 15) & 0x1f
      val rs2: Int = (instr >> 20) & 0x1f

      var imm_I: Int = (instr >> 20) & 0xfff
      //println("imm_I just loaded: "+imm_I.toHexString);
      if((imm_I>>11)==1) imm_I = imm_I ^ 0xfffff000                 // Check if negative
      //println("imm_I after neg check: "+imm_I.toHexString);

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
          case 0x3 => if (reg(rd) < imm_I) {                        //SLTIU -- Korrekt??????
            reg(rd) = 1
          } else {
            reg(rd)
          }
          case 0x4 => reg(rd) = reg(rs1) ^ imm_I                    //XORI
          case 0x5 => func7 match {
            case 0x0 => reg(rd) = reg(rs1) >>> imm_I                 //SRLI
            case 0x20 => reg(rd) = reg(rs1) >> imm_I                //SRAI
            case _ => println("ERROR7" + func3)
          }
          case 0x6 => reg(rd) = reg(rs1) | imm_I                    //ORI
          case 0x7 => reg(rd) = reg(rs1) & imm_I                    //ANDI
          case _ => println("ERROR3_1" + func3)
        }
        case 0x17 => reg(rd) = pc + (imm_U << 12)                   //AUIPC
        case 0x33 => func3 match { //R
          case 0x0 => func7 match { //f7
            case 0x0 => reg(rd) = reg(rs1) + reg(rs2)               //ADD
            case 0x20 => reg(rd) = reg(rs1) - reg(rs2)              //SUB
            case _ => println("ERROR7_1" + func7)
          }
          case 0x1 => reg(rd) = reg(rs1) << reg(rs2)                //SLL
          case 0x2 => if (reg(rs1) < reg(rs2)) {                    //SLT
            reg(rd) = 1
          } else {
            reg(rd) = 0
          }
          case 0x3 => if (reg(rs1) < reg(rs2)) {                    //SLTU  -- Korrekt?????
            reg(rd) = 1
          } else {
            reg(rd) = 0
          }
          case 0x4 => reg(rd) = reg(rs1) ^ reg(rs2)                 //XOR
          case 0x5 => func7 match { //f7
            case 0x0 => reg(rd) = reg(rs1) >>> reg(rs2)             //SRL
            case 0x20 => reg(rd) = reg(rs1) >> reg(rs2)             //SRA
            case _ => println("ERROR7_2" + func7)
          }
          case 0x6 => reg(rd) = reg(rs1) | reg(rs2)                 //OR
          case 0x7 => reg(rd) = reg(rs1) & reg(rs2)                 //AND
          case _ => println("ERROR3_1" + func3)
        } //R done
        case 0x37 => reg(rd) = imm_U << 12                          //LUI
        case 0x23 => func3 match {                                              // Store instructions
          case 0x0 => mem(reg(rs1) + imm_S) = reg(rs2) & 0xFF       //SB
          case 0x1 => mem(reg(rs1) + imm_S) = reg(rs2) & 0xFFFF     //SH
          case 0x2 => {
            mem(reg(rs1) + imm_S) = reg(rs2)              //SW
            //println("SW debug, addr: "+ (reg(rs1) + imm_S))
          }
          case _ => println("ERROR3_1" + func3)
        }
        case 0x3 => func3 match {                                               //Load instructions
          case 0x0 => reg(rd) = mem.getOrElse(reg(rs1) + imm_I, 0) & 0XFF        //LB
          case 0x1 => reg(rd) = mem.getOrElse(reg(rs1) + imm_I, 0) & 0XFFFF      //LH
          case 0x2 => {
            reg(rd) = mem.getOrElse(reg(rs1) + imm_I, 0)               //LW
            //println("LW reg del, addr: "+ reg(rs1))
            //println("LW, imm_I del, addr: "+imm_I)
            //println("LW debug, addr: "+ (reg(rs1) + imm_I))
          }
          case 0x4 => reg(rd) = mem.getOrElse(reg(rs1) + imm_I, 0) & 0XFF        //LBU -- KORREKT????
          case 0x5 => reg(rd) = mem.getOrElse(reg(rs1) + imm_I, 0) & 0XFFFF      //LHU -- KORREKT????
          case _ => println("ERROR3_1" + func3)
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
          case 0x6 => if ((reg(rs1) & 0x7fffffff) < (reg(rs2) & 0x7fffffff)) {                    //BLTU   -- Korrekt?????
            pc = pc + imm_SB - 4
          }
          case 0x7 => if ((reg(rs1) & 0x7fffffff) >= (reg(rs2) & 0x7fffffff)) {                   //BGEU   -- Korrekt?????
            pc = pc + imm_SB - 4
          }
          case _ => println("ERROR3_1" + func3)
        }
        case 0x67 => func3 match {
          case 0x0 =>                                               //JALR     -- Korrekt?????
            reg(rd) = pc + 4
            pc = (reg(rs1)+imm_I) - 4
          case _ => println("ERROR3_1" + func3)
        }
        case 0x6f =>                                                //JAL
          reg(rd) = pc + 4
          pc = pc + imm_UJ - 4
        case 0x73 =>  reg(10) match {                                             //ECALL
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

      for (i <- reg.indices) print(reg(i) + " ")                    //used for debugging
      println()
      //println("Memory")
      //for ((k,v) <- mem) printf("key: %s, value: %s\n", k, v)
      //println()
      if (pc >= programLength) {
        println("ended because pc was: "+pc/4)
        loopBreak = 1
      }
    }
    println("Register overview (decimal):")
    for (i <- reg.indices) print(reg(i) + " ")
    println("\n")

    println("Register overview (hex):")
    for (i <- 10 to 17) {
      print("%02d".format(i)+": ")
      (1 to (8-reg(i).toHexString.length())).foreach(_ => print("0"))
      print(reg(i).toHexString + "\n")
    }
    println("\n")
    println("reg.length: "+reg.length)
    println("\n")
/*
    println("Res file registers");

    var bc: Int = 0
    var resLoopBreak: Int = 0
    val resByteArray = Files.readAllBytes(Paths.get("addlarge.res"))
    var res: HashMap[Int,Int] = new HashMap[Int,Int]()               //program array
    while (resLoopBreak < resByteArray.length) {
      val byteStr: Int = ((resByteArray(programLoopBreak + 3 & 0xff) << 24) + ((resByteArray(programLoopBreak + 2) & 0xff) << 16) + ((resByteArray(programLoopBreak + 1) & 0xff) << 8) + ((resByteArray(programLoopBreak)) & 0xff))
      res(bc) = byteStr
      bc = bc + 4                                                   //Save instructions in a 32bit integer
      resLoopBreak = resLoopBreak + 4
    }

    println("Res overview (hex):")
    for (i <- 0 to resByteArray.length/4-4) {
      print("%02d".format(i)+": ")
      (1 to (8-res(i*4).toHexString.length())).foreach(_ => print("0"))
      print(res(i*4).toHexString + "\n")
    }
    println("\n")
*/
    println("Program exit")
  }
}
