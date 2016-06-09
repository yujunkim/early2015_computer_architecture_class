import Multiplexer::*;

interface BarrelShifterRight;
  method ActionValue#(Bit#(32)) rightShift(Bit#(32) val, Bit#(5) shiftAmt, Bit#(1) shiftValue);
endinterface

module mkBarrelShifterRight(BarrelShifterRight);
  method ActionValue#(Bit#(32)) rightShift(Bit#(32) val, Bit#(5) shiftAmt, Bit#(1) shiftValue);
    for (Integer i = 0; i < 5; i = i + 1)
    begin
      Bit#(32) temp = val;
      for (Integer j = 0; j < 2**i; j = j + 1)
        temp = {shiftValue, temp[31:1]};
      val = multiplexer32(shiftAmt[i], val, temp);
    end
    return val;
  endmethod
endmodule

interface BarrelShifterRightLogical;
  method ActionValue#(Bit#(32)) rightShift(Bit#(32) val, Bit#(5) shiftAmt);
endinterface

module mkBarrelShifterRightLogical(BarrelShifterRightLogical);
  let bsr <- mkBarrelShifterRight;
  method ActionValue#(Bit#(32)) rightShift(Bit#(32) val, Bit#(5) shiftAmt);
    // return (val >> shiftAmt);
    Bit#(32) x = 0;
    x <- bsr.rightShift(val, shiftAmt, 0);
    return x;
  endmethod
endmodule

typedef BarrelShifterRightLogical BarrelShifterRightArithmetic;

module mkBarrelShifterRightArithmetic(BarrelShifterRightArithmetic);
  let bsr <- mkBarrelShifterRight;
  method ActionValue#(Bit#(32)) rightShift(Bit#(32) val, Bit#(5) shiftAmt);
    // Int#(32) newVal = unpack(val);
    // return pack(newVal >> shiftAmt);
    Bit#(32) x = 0;
    x <- bsr.rightShift(val, shiftAmt, val[31]);
    return x;
  endmethod
endmodule

