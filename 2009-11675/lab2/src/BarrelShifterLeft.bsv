import BarrelShifterRight::*;

interface BarrelShifterLeft;
	method ActionValue#(Bit#(32)) leftShift(Bit#(32) val, Bit#(5) shiftAmt);
endinterface

module mkBarrelShifterLeft(BarrelShifterLeft);
	let bsr <- mkBarrelShifterRightLogical;
	method ActionValue#(Bit#(32)) leftShift(Bit#(32) val, Bit#(5) shiftAmt);
		/* Remove the below line and implement a left shifter using the given logical right shifter */
    Bit#(32) x = 0;
    x <- bsr.rightShift(reverseBits(val), shiftAmt);
    return reverseBits(x);
	endmethod
endmodule
