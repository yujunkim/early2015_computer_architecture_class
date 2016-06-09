import Vector::*;

import FftCommon::*;
import Fifo::*;

interface Fft;
  method Action enq(Vector#(FftPoints, ComplexData) in);
  method ActionValue#(Vector#(FftPoints, ComplexData)) deq;
endinterface

(* synthesize *)
module mkFftCombinational(Fft);
  Fifo#(2,Vector#(FftPoints, ComplexData)) inFifo <- mkCFFifo;
  Fifo#(2,Vector#(FftPoints, ComplexData)) outFifo <- mkCFFifo;
  Vector#(NumStages, Vector#(BflysPerStage, Bfly4)) bfly <- replicateM(replicateM(mkBfly4));

  function Vector#(FftPoints, ComplexData) stage_f(StageIdx stage, Vector#(FftPoints, ComplexData) stage_in);
    Vector#(FftPoints, ComplexData) stage_temp, stage_out;
    for (FftIdx i = 0; i < fromInteger(valueOf(BflysPerStage)); i = i + 1)
    begin
      FftIdx idx = i * 4;
      Vector#(4, ComplexData) x;
      Vector#(4, ComplexData) twid;
      for (FftIdx j = 0; j < 4; j = j + 1 )
      begin
        x[j] = stage_in[idx+j];
        twid[j] = getTwiddle(stage, idx+j);
      end
      let y = bfly[stage][i].bfly4(twid, x);

      for(FftIdx j = 0; j < 4; j = j + 1 )
        stage_temp[idx+j] = y[j];
    end

    stage_out = permute(stage_temp);

    return stage_out;
  endfunction

  rule doFft;
    inFifo.deq;
    Vector#(4, Vector#(FftPoints, ComplexData)) stage_data;
    stage_data[0] = inFifo.first;

    for (StageIdx stage = 0; stage < 3; stage = stage + 1)
      stage_data[stage+1] = stage_f(stage, stage_data[stage]);
    outFifo.enq(stage_data[3]);
  endrule

  method Action enq(Vector#(FftPoints, ComplexData) in);
    inFifo.enq(in);
  endmethod

  method ActionValue#(Vector#(FftPoints, ComplexData)) deq;
    outFifo.deq;
    return outFifo.first;
  endmethod
endmodule

(* synthesize *)
module mkFftFolded(Fft);
  Fifo#(2,Vector#(FftPoints, ComplexData)) inFifo <- mkCFFifo;
  Fifo#(2,Vector#(FftPoints, ComplexData)) outFifo <- mkCFFifo;
  Vector#(16, Bfly4) bfly <- replicateM(mkBfly4);
  Reg#(Vector#(FftPoints, ComplexData)) sReg <- mkRegU();
  Reg#(StageIdx) s <- mkReg(0);
  function Vector#(FftPoints, ComplexData) stage_f(StageIdx stage, Vector#(FftPoints, ComplexData) stage_in);
    Vector#(FftPoints, ComplexData) stage_temp, stage_out;
    for (FftIdx i = 0; i < 16; i = i + 1)
    begin
      FftIdx idx = i * 4;
      Vector#(4, ComplexData) x;
      Vector#(4, ComplexData) twid;
      for (FftIdx j = 0; j < 4; j = j + 1 )
      begin
        x[j] = stage_in[idx+j];
        twid[j] = getTwiddle(stage, idx+j);
      end
      let y = bfly[i].bfly4(twid, x);

      for(FftIdx j = 0; j < 4; j = j + 1 )
        stage_temp[idx+j] = y[j];
    end

    stage_out = permute(stage_temp);

    return stage_out;
  endfunction

  rule foldedEndry if (s == 0);
    sReg <= stage_f(s, inFifo.first);
    s <= s + 1;
    inFifo.deq;
  endrule

  rule foldedCirculate if ((s != 0)&&(s < 2));
    sReg <= stage_f(s, sReg);
    s <= s + 1;
  endrule

  rule foldedExit if (s == 2);
    outFifo.enq(stage_f(s, sReg));
    s <= 0;
  endrule

  method Action enq(Vector#(FftPoints, ComplexData) in);
    inFifo.enq(in);
  endmethod

  method ActionValue#(Vector#(FftPoints, ComplexData)) deq;
    outFifo.deq;
    return outFifo.first;
  endmethod
endmodule

(* synthesize *)
module mkFftPipelined(Fft);
  Fifo#(2,Vector#(FftPoints, ComplexData)) inFifo <- mkBypassFifo;
  Fifo#(2,Vector#(FftPoints, ComplexData)) outFifo <- mkBypassFifo;
  Vector#(3, Vector#(16, Bfly4)) bfly <- replicateM(replicateM(mkBfly4));
  Reg#(Maybe#(Vector#(FftPoints, ComplexData))) sReg1 <- mkRegU();
  Reg#(Maybe#(Vector#(FftPoints, ComplexData))) sReg2 <- mkRegU();

  function Vector#(FftPoints, ComplexData) stage_f(StageIdx stage, Vector#(FftPoints, ComplexData) stage_in);
    Vector#(FftPoints, ComplexData) stage_temp, stage_out;
    for (FftIdx i = 0; i < fromInteger(valueOf(BflysPerStage)); i = i + 1)
    begin
      FftIdx idx = i * 4;
      Vector#(4, ComplexData) x;
      Vector#(4, ComplexData) twid;
      for (FftIdx j = 0; j < 4; j = j + 1 )
      begin
        x[j] = stage_in[idx+j];
        twid[j] = getTwiddle(stage, idx+j);
      end
      let y = bfly[stage][i].bfly4(twid, x);

      for(FftIdx j = 0; j < 4; j = j + 1 )
        stage_temp[idx+j] = y[j];
    end

    stage_out = permute(stage_temp);

    return stage_out;
  endfunction

  rule doFft;
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
      tagged Valid .sx2: outFifo.enq(stage_f(2, sx2));
    endcase
  endrule

  method Action enq(Vector#(FftPoints, ComplexData) in);
    inFifo.enq(in);
  endmethod

  method ActionValue#(Vector#(FftPoints, ComplexData)) deq;
    outFifo.deq;
    return outFifo.first;
  endmethod
endmodule

interface SuperFoldedFft#(numeric type radix);
  method ActionValue#(Vector#(FftPoints, ComplexData)) deq;
  method Action enq(Vector#(FftPoints, ComplexData) in);
endinterface

module mkFftSuperFolded(SuperFoldedFft#(radix)) provisos(Div#(TDiv#(FftPoints, 4), radix, times), Mul#(radix, times, TDiv#(FftPoints, 4)));
  Fifo#(2,Vector#(FftPoints, ComplexData)) inFifo <- mkCFFifo;
  Fifo#(2,Vector#(FftPoints, ComplexData)) outFifo <- mkCFFifo;
  Vector#(radix, Bfly4) bfly <- replicateM(mkBfly4);

  Reg#(Vector#(FftPoints, ComplexData)) sReg <- mkRegU();
  Reg#(StageIdx) s <- mkReg(0);
  Reg#(FftIdx) bs <- mkReg(0);

  function Vector #(FftPoints, ComplexData) bstage_f(FftIdx bstage, StageIdx stage, Vector#(FftPoints, ComplexData) stage_in);
    Vector#(FftPoints, ComplexData) stage_temp;
    stage_temp = stage_in;
    for (FftIdx i = 0; i < fromInteger(valueOf(radix)); i = i + 1)
    begin
      FftIdx idx = bstage * fromInteger(valueOf(radix)) * 4 + i * 4;
      Vector#(4, ComplexData) x;
      Vector#(4, ComplexData) twid;

      for (FftIdx j = 0; j < 4; j = j + 1 )
      begin
        x[j] = stage_in[idx+j];
        twid[j] = getTwiddle(stage, idx+j);
      end

      let y = bfly[i].bfly4(twid, x);

      for(FftIdx j = 0; j < 4; j = j + 1 )
        stage_temp[idx+j] = y[j];

    end

    return stage_temp;
  endfunction

  rule bsNotZeroRule if (fromInteger(valueOf(radix)) != 16 && bs != 0);
    if (((bs + 1) * fromInteger(valueOf(radix))) == 16)
    begin
      if (s == 2)
      begin
        outFifo.enq(permute(bstage_f(bs, s, sReg)));
        s <= 0;
      end
      else
      begin
        sReg <= permute(bstage_f(bs, s, sReg));
        s <= s + 1;
      end
      bs <= 0;
    end
    else
    begin
      sReg <= bstage_f(bs, s, sReg);
      bs <= bs + 1;
    end
  endrule

  rule foldedEntry if (fromInteger(valueOf(radix)) != 16 && bs == 0 && s == 0);
    sReg <= bstage_f(bs, s, inFifo.first);
    bs <= bs + 1;
    inFifo.deq;
  endrule

  rule foldedCirculate if (fromInteger(valueOf(radix)) != 16 && bs == 0 && s == 1);
    sReg <= bstage_f(bs, s, sReg);
    bs <= bs + 1;
  endrule

  rule foldedExit if (fromInteger(valueOf(radix)) != 16 && bs == 0 && s == 2);
    sReg <= bstage_f(bs, s, sReg);
    bs <= bs + 1;
  endrule

  rule radixSixteenFoldedEntry if (fromInteger(valueOf(radix)) == 16 && s == 0);
    sReg <= permute(bstage_f(0, s, inFifo.first));
    inFifo.deq;
    s <= s + 1;
  endrule

  rule radixSixteenFoldedCirculate if (fromInteger(valueOf(radix)) == 16 && s == 1);
    sReg <= permute(bstage_f(0, s, sReg));
    s <= s + 1;
  endrule

  rule radixSixteenFoldedExit if (fromInteger(valueOf(radix)) == 16 && s == 2);
    outFifo.enq(permute(bstage_f(0, s, sReg)));
    s <= 0;
  endrule

  method Action enq(Vector#(FftPoints, ComplexData) in);
    inFifo.enq(in);
  endmethod

  method ActionValue#(Vector#(FftPoints, ComplexData)) deq;
    outFifo.deq;
    return outFifo.first;
  endmethod
endmodule

function Fft getFft(SuperFoldedFft#(radix) f);
  return (interface Fft;
    method enq = f.enq;
    method deq = f.deq;
  endinterface);
endfunction
