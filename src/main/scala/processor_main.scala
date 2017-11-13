import scala.util.control.Breaks._
import java.nio.file.{Files, Paths}
import java.util
import collection.mutable.HashMap

object processor_main {
  var pc: Int = 0
  var lc: Int = 0

  val reg: Array[Int] = Array.fill[Int](31)(0)

  var mem: HashMap[Int,Int] = new HashMap[Int,Int]() //Memory - kunne det klares med en Byte????

  // Here the first program hard coded as an array
//  var progr = Array[Int](0x00200093, 0x00300113, 0x13051200, 0x02532423)
//var progr = Array[Int](0x7d000293, 0x02532423, 0x02832303) // load store test program
//var progr = Array[Int](0x014002130, 0x00402023) // load store test program
  var progr = Array[Int](0x80100293, 0x0052a023, 0x0002a303)

 // var progr = Array[Int](0x02532423)

  //val byteArrayTest = Files.readAllLines(Paths.get("branchcnt.bin"))
  // As minimal RISC-V assembler example

  def main(args: Array[String]){
    println("Hello RISC-V World!")
    //println(byteArrayTest(1))
    pc = 0
    while (lc==0) {
      val instr: Int = progr(pc)
      val opcode: Int = instr & 0x7f
      val rd: Int = (instr >> 7) & 0x1f
      val func3: Int = (instr >> 12) & 0x7
      val func7: Int = (instr >> 25) & 0x7f
      val rs1: Int = (instr >> 15) & 0x1f
      val rs2: Int = (instr >> 20) & 0x1f
      val imm_I: Int = (instr >> 20) & 0xfff
      val imm_S: Int = (((instr >> 25) & 0x7f) << 5) + ((instr >> 7) & 0x1F)
      //var imm_SB: Int =
      //var imm_U:
      //var imm_UJ:

      opcode match {
        case 0x13 => func3 match {
          case 0x0 => reg(rd) = reg(rs1) + imm_I                      //ADDI
          case 0x1 => reg(rd) = reg(rs1) << imm_I                     //SLLI
          case 0x2 => if (reg(rd)<reg(rs1)) {                           //SLTI
            reg(rd) = 1
          } else {
            reg(rd) = 0
          }
          case 0x3 => if (reg(rd)<reg(rs1)) {                           //SLTIU -- Korrekt??????
            reg(rd) = 1
          } else {
            reg(rd)
          }
          case 0x4 => reg(rd) = reg(rs1) ^ imm_I                      //XORI
          case 0x5 => func7 match {
            case 0x0 => reg(rd) = reg(rs1) >> imm_I                      //SRLI
            case 0x1 => reg(rd) = reg(rs1) >>> imm_I                      //SRAI -- Korrekt?????
          }
          case 0x6 => reg(rd) = reg(rs1) | imm_I                      //ORI
          case 0x7 => reg(rd) = reg(rs1) & imm_I                      //ANDI
        }
        case 0x17 => reg(rd) = pc + imm_I                              //AUIPC -- Korrekt?????
        case 0x33 => func3 match {                           //R
          case 0x0 => func7 match {                    //f7
            case 0x0 => reg(rd) = reg(rs1) + reg(rs2)               //ADD
            case 0x4 => reg(rd) = reg(rs1)^reg(rs2)                 //XOR
            case 0x20 => reg(rd) = reg(rs1) - reg(rs2)              //SUB
            case _ => println("ERROR7_1"+func7)
          }
          case 0x1 => reg(rd) = reg(rs1) << reg(rs2)                //SLL
          case 0x2 => if(reg(rs1)<reg(rs2)){                        //SLT
            reg(rd) = 1
          } else{
            reg(rd) = 0
          }
          case 0x3 => if(reg(rs1)<(imm_I&0xFF)){                     //SLTU
            reg(rd) = 1
          } else{
            reg(rd) = 0
          }
          case 0x4 => reg(rd) = reg(rs1) ^ reg(rs2)                 //SLTU
          case 0x5 => func7 match {                    //f7
            case 0x0 => reg(rd) = reg(rs1) >>> reg(rs2)             //SRL
            case 0x20 => reg(rd) = reg(rs1) >> reg(rs2)             //SRA
            case _ => println("ERROR7_2"+func7)
          }
          case 0x6 => reg(rd) = reg(rs1) | reg(rs2)                 //OR
          case 0x7 => reg(rd) = reg(rs1) & reg(rs2)                 //AND
          case _ => println("ERROR3_1"+func3)
        }                                                    //R done
        case 0x37 => reg(rd) = imm_I<<16                             //LUI
        case 0x23 => func3 match {                            // Store instructions
          case 0x0 => mem(reg(rs1)+imm_S) = reg(rs2)&0xFF // SB
          case 0x1 => mem(reg(rs1)+imm_S) = reg(rs2)&0xFFFF// SH
          case 0x2 => mem(reg(rs1)+imm_S) = reg(rs2) //SW
        }
        case 0x3 => func3 match {                             //Load instructions
          case 0x0 => reg(rd) = mem(reg(rs1)+ imm_I)&0XFF //LB
          case 0x1 => reg(rd) = mem(reg(rs1)+ imm_I)&0XFFFF //LH
          case 0x2 => reg(rd) = mem(reg(rs1)+ imm_I) //LW
          case 0x4 => reg(rd) = mem(reg(rs1)+ imm_I)&0XFF //LBU -- KORREKT????
          case 0x5 => reg(rd) = mem(reg(rs1)+ imm_I)&0XFFFF //LHU -- KORREKT????
        }
        case _ => {
          println("Opcode " + opcode + " not yet implemented:")
          println("Opcode = " + opcode)
          println("rd = " + rd)
          println("func3 = " + func3)
          println("rs1 = " + rs1)
          println("rs2 = " + rs2)
          println("imm_I = " + imm_I)
          println("func7 = " + func7)
          println("imm_S = " + imm_S)
        }
      }


      pc = pc + 1
     // for (i <- reg.indices) print(String.format("%4s", reg(i).toHexString).replace(' ', '0') + " ") // reg.indices = 0 until reg.length
      for (i <- reg.indices) print(reg(i).toHexString + " ") // reg.indices = 0 until reg.length
      println()
      if (pc >= progr.length) {lc = 1}
    }
  }
  println("Program exit")
}
