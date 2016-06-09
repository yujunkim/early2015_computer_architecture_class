import Multiplexer::*;
import FIFO::*;
import FIFOF::*;
import Vector::*;
import SpecialFIFOs::*;

/* Interface of the basic right shifter module */
interface BarrelShifterRightPipelined;
  method Action shift_request(Bit#(32) operand, Bit#(5) shamt, Bit#(1) val);
  method ActionValue#(Bit#(32)) shift_response();
endinterface

/* Interface of the three shifter modules
 *
 * They have the same interface.
 * So, we just copy it using typedef declarations.
 */
interface BarrelShifterRightLogicalPipelined;
  method Action shift_request(Bit#(32) operand, Bit#(5) shamt);
  method ActionValue#(Bit#(32)) shift_response();
endinterface

typedef BarrelShifterRightLogicalPipelined BarrelShifterRightArithmeticPipelined;
typedef BarrelShifterRightLogicalPipelined BarrelShifterLeftPipelined;

module mkBarrelShifterLeftPipelined(BarrelShifterLeftPipelined);
  /* Implement left shifter using the pipelined right shifter. */
  let bsrp <- mkBarrelShifterRightPipelined;

  method Action shift_request(Bit#(32) operand, Bit#(5) shamt);
    bsrp.shift_request(reverseBits(operand), shamt, 0);
  endmethod

  method ActionValue#(Bit#(32)) shift_response();
    Bit#(32) x = 0;
    x <- bsrp.shift_response();
    return reverseBits(x);
  endmethod
endmodule

module mkBarrelShifterRightLogicalPipelined(BarrelShifterRightLogicalPipelined);
  /* Implement right logical shifter using the pipelined right shifter. */
  let bsrp <- mkBarrelShifterRightPipelined;

  method Action shift_request(Bit#(32) operand, Bit#(5) shamt);
    bsrp.shift_request(operand, shamt, 0);
  endmethod

  method ActionValue#(Bit#(32)) shift_response();
    Bit#(32) x = 0;
    x <- bsrp.shift_response();
    return x;
  endmethod
endmodule

module mkBarrelShifterRightArithmeticPipelined(BarrelShifterRightArithmeticPipelined);
  /* Implement right arithmetic shifter using the pipelined right shifter. */
  let bsrp <- mkBarrelShifterRightPipelined;

  method Action shift_request(Bit#(32) operand, Bit#(5) shamt);
    bsrp.shift_request(operand, shamt, operand[31]);
  endmethod

  method ActionValue#(Bit#(32)) shift_response();
    Bit#(32) x = 0;
    x <- bsrp.shift_response();
    return x;
  endmethod
endmodule

module mkBarrelShifterRightPipelined(BarrelShifterRightPipelined);

  let inFifo <- mkFIFOF;
  let outFifo <- mkFIFOF;
  Reg#(Maybe#(Tuple3#(Bit#(32), Bit#(5), Bit#(1)))) sReg1 <- mkRegU();
  Reg#(Maybe#(Tuple3#(Bit#(32), Bit#(5), Bit#(1)))) sReg2 <- mkRegU();
  Reg#(Maybe#(Tuple3#(Bit#(32), Bit#(5), Bit#(1)))) sReg3 <- mkRegU();
  Reg#(Maybe#(Tuple3#(Bit#(32), Bit#(5), Bit#(1)))) sReg4 <- mkRegU();

  function stage_f(Bit#(3) stage, Tuple3#(Bit#(32), Bit#(5), Bit#(1)) tp);
    Bit#(32) val = tpl_1(tp);
    Bit#(32) temp = val;
    for (Integer j = 0; j < 2**stage; j = j + 1)
      temp = {tpl_3(tp), temp[31:1]};
    val = multiplexer32(tpl_2(tp)[stage], val, temp);
    return tuple3(val, tpl_2(tp), tpl_3(tp));
  endfunction

  rule shift;
    if (inFifo.notEmpty())
    begin
      sReg1 <= Valid (stage_f(0, inFifo.first()));
      inFifo.deq;
    end
    case (sReg1) matches
      tagged Valid .sx1: sReg2 <= Valid (stage_f(1, sx1));
      tagged Invalid: sReg2 <= Invalid;
    endcase
    case (sReg2) matches
      tagged Valid .sx2: sReg3 <= Valid (stage_f(2, sx2));
      tagged Invalid: sReg3 <= Invalid;
    endcase
    case (sReg3) matches
      tagged Valid .sx3: sReg4 <= Valid (stage_f(3, sx3));
      tagged Invalid: sReg4 <= Invalid;
    endcase
    case (sReg4) matches
      tagged Valid .sx4: outFifo.enq(tpl_1(stage_f(4, sx4)));
    endcase
  endrule

  method Action shift_request(Bit#(32) operand, Bit#(5) shamt, Bit#(1) val);
    inFifo.enq(tuple3(operand, shamt, val));
  endmethod

  method ActionValue#(Bit#(32)) shift_response();
    outFifo.deq;
    return outFifo.first;
  endmethod
endmodule
