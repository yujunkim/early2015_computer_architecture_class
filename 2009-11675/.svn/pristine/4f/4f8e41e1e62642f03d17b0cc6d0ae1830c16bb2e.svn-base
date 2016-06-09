import Types::*;
import ProcTypes::*;
import MemTypes::*;
import BypassRFile::*;
//import BypassRFile::*;
import IMemory::*;
import DMemory::*;
import Decode::*;
import Exec::*;
import Cop::*;
import Fifo::*;
import Scoreboard::*;

typedef struct {
  Inst inst;
  Addr pc;
  Addr ppc;
  int epoch;
} Fetch2Decode deriving(Bits, Eq);

typedef struct {
  DecodedInst dInst;
  Addr ppc;
  int epoch;
} Decode2Execute deriving(Bits, Eq);

typedef struct {
  Maybe#(FullIndx) adr;
  Maybe#(Data) d;
} ForwardUnit deriving(Bits, Eq);


(*synthesize*)
module mkProc(Proc);
  Reg#(Addr)    pc  <- mkRegU;
  RFile         rf  <- mkBypassRFile;
  IMemory     iMem  <- mkIMemory;
  DMemory     dMem  <- mkDMemory;
  Cop          cop  <- mkCop;

  Reg#(CondFlag)    condFlag  <- mkRegU;
  Reg#(ProcStatus)    stat    <- mkRegU;

  Fifo#(1,Addr)       execRedirect <- mkBypassFifo;
  Fifo#(1,Addr)       memRedirect <- mkBypassFifo;
  Fifo#(1,ProcStatus) statRedirect <- mkBypassFifo;

  Fifo#(1,ForwardUnit) efe <- mkBypassFifo;
  Fifo#(1,ForwardUnit) efm <- mkBypassFifo;
  Fifo#(1,ForwardUnit) mfm <- mkBypassFifo;

  Fifo#(1,Maybe#(ExecInst)) mmm        <- mkBypassFifo;

  Fifo#(1,Fetch2Decode) f2d        <- mkPipelineFifo;
  Fifo#(1,Decode2Execute) d2e        <- mkPipelineFifo;
  Fifo#(1,Maybe#(ExecInst)) e2m        <- mkPipelineFifo;
  Fifo#(1,Maybe#(ExecInst)) m2w        <- mkPipelineFifo;

  Reg#(int) fEpoch <- mkRegU;
  Reg#(int) eEpoch <- mkRegU;

  rule doFetch(cop.started && stat == AOK);
    let epoch = fEpoch;
    let p = pc;
    if(execRedirect.notEmpty)
    begin
      epoch = epoch + 1;
      execRedirect.deq;
      p = execRedirect.first;
    end

    fEpoch <= epoch;

    let inst = iMem.req(p);
    let ppc = nextAddr(p, getICode(inst));

    $display("Fetch : from Pc %d , expanded inst : %x, \n", p, inst, showInst(inst));

    pc <= ppc;
    f2d.enq(Fetch2Decode{inst:inst, pc:p, ppc:ppc, epoch:epoch});

  endrule

  rule doDecode(cop.started && stat == AOK);
    let inst   = f2d.first.inst;
    let ipc    = f2d.first.pc;
    let ppc    = f2d.first.ppc;
    let iEpoch = f2d.first.epoch;

    //Decode
    let dInst = decode(inst, ipc);
    $display("Decode : from Pc %d , expanded inst : %x, \n", ipc, inst, showInst(inst));


    dInst.valA   = isValid(dInst.regA)? tagged Valid rf.rdA(validRegValue(dInst.regA)) : Invalid;
    dInst.valB   = isValid(dInst.regB)? tagged Valid rf.rdB(validRegValue(dInst.regB)) : Invalid;

    if(efm.notEmpty) begin
      let fu = efm.first;
      efm.deq;
      if (isValid(fu.adr) && isValid(fu.d)) begin
        if (fu.adr == dInst.regA) begin
          dInst.valA = fu.d;
        end
        if (fu.adr == dInst.regB) begin
          dInst.valB = fu.d;
        end
      end
    end

    if(mfm.notEmpty) begin
      let fu = mfm.first;
      mfm.deq;
      if (isValid(fu.adr) && isValid(fu.d)) begin
        if (fu.adr == dInst.regA) begin
          dInst.valA = fu.d;
        end
        if (fu.adr == dInst.regB) begin
          dInst.valB = fu.d;
        end
      end
    end

    if(efe.notEmpty) begin
      let fu = efe.first;
      efe.deq;
      if (isValid(fu.adr) && isValid(fu.d)) begin
        if (fu.adr == dInst.regA) begin
          dInst.valA = fu.d;
        end
        if (fu.adr == dInst.regB) begin
          dInst.valB = fu.d;
        end
      end
    end


    dInst.copVal = isValid(dInst.regA)? tagged Valid cop.rd(validRegValue(dInst.regA)) : Invalid;

    if (mmm.notEmpty) begin
      let eInst = validValue(mmm.first);
      mmm.deq;
      if ((eInst.iType == MRmov || eInst.iType == Pop) &&
        (eInst.dstM == dInst.regA || eInst.dstM == dInst.regB)) begin
      end
      else
      begin
        d2e.enq(Decode2Execute{dInst:dInst, ppc:ppc, epoch:iEpoch});
        f2d.deq;
      end
    end
    else
    begin
      d2e.enq(Decode2Execute{dInst:dInst, ppc:ppc, epoch:iEpoch});
      f2d.deq;
    end

  endrule

  rule doExecute(cop.started && stat == AOK);
    let dInst   = d2e.first.dInst;
    let iEpoch   = d2e.first.epoch;
    let ppc    = d2e.first.ppc;

    if(memRedirect.notEmpty)
    begin
      let mrpc = memRedirect.first;
      memRedirect.deq;
      eEpoch <= eEpoch + 1;
      execRedirect.enq(mrpc);
      e2m.enq(Invalid);
    end
    else if(iEpoch == eEpoch)
    begin
      let eInst = exec(dInst, condFlag, ppc);

      condFlag <= eInst.condFlag;


      if(!memRedirect.notEmpty && eInst.mispredict)
      begin
        let redirPc = validValue(eInst.nextPc);
        $display("mispredicted, redirect %d ", redirPc);
        eEpoch <= eEpoch + 1;
        execRedirect.enq(redirPc);
      end

      mmm.enq(Valid(eInst));
      efe.enq(ForwardUnit{adr: eInst.dstE, d: eInst.valE});
      e2m.enq(Valid(eInst));
    end
    else
    begin
      e2m.enq(Invalid);
    end

    d2e.deq;

  endrule

  rule doMemory(cop.started && stat == AOK);
    if (isValid(e2m.first))
    begin
      let eInst   = validValue(e2m.first);

      let iType = eInst.iType;

      case(iType)
        MRmov, Pop, Ret :
        begin
          let ldData <- (dMem.req(MemReq{op: Ld, addr: eInst.memAddr, data:?}));
          eInst.valM = Valid(little2BigEndian(ldData));
          $display("Loaded %d from %d",little2BigEndian(ldData), eInst.memAddr);
          if(iType == Ret)
          begin
            eInst.nextPc = eInst.valM;

            let redirPc = validValue(eInst.nextPc);
            $display("mispreret. redirect %d ", redirPc);
            memRedirect.enq(redirPc);
          end
        end

        RMmov, Call, Push :
        begin
          let stData = (iType == Call)? eInst.valP : validValue(eInst.valA);
          let dummy <- dMem.req(MemReq{op: St, addr: eInst.memAddr, data: big2LittleEndian(stData)});
          $display("Stored %d into %d",stData, eInst.memAddr);
        end
      endcase

      efm.enq(ForwardUnit{adr: eInst.dstE, d: eInst.valE});
      mfm.enq(ForwardUnit{adr: eInst.dstM, d: eInst.valM});
      m2w.enq(Valid(eInst));
    end
    else
    begin
      m2w.enq(Invalid);
    end
    e2m.deq;

  endrule

  rule doWriteBack(cop.started && stat == AOK);
    if (isValid(m2w.first))
    begin

      let eInst   = validValue(m2w.first);

      //Update Status
      let newStatus = case(eInst.iType)
                Unsupported : INS;
                Hlt       : HLT;
                default     : AOK;
              endcase;
      statRedirect.enq(newStatus);

      //WriteBack
      if(isValid(eInst.dstE))
      begin
        $display("On %d, writes %d   (wrE)",validRegValue(eInst.dstE), validValue(eInst.valE));
        rf.wrE(validRegValue(eInst.dstE), validValue(eInst.valE));
      end
      if(isValid(eInst.dstM))
      begin
        $display("On %d, writes %d   (wrM)",validRegValue(eInst.dstM), validValue(eInst.valM));
        rf.wrM(validRegValue(eInst.dstM), validValue(eInst.valM));
      end

      cop.wr(eInst.dstE, validValue(eInst.valE));
    end
    m2w.deq;
  endrule

  rule upd_Stat(cop.started);
    $display("Stat update");
    statRedirect.deq;
    stat <= statRedirect.first;
  endrule

  rule statHLT(cop.started && stat == HLT);
    $fwrite(stderr,"Program Finished by halt\n");
    $finish;
  endrule

  rule statINS(cop.started && stat == INS);
    $fwrite(stderr,"Executed unsupported instruction. Exiting\n");
    $finish;
  endrule

  method ActionValue#(Tuple3#(RIndx, Data, Data)) cpuToHost;
    let retV <- cop.cpuToHost;
    return retV;
  endmethod

  method Action hostToCpu(Bit#(32) startpc) if (!cop.started);
    cop.start;
    eEpoch <= 0;
    fEpoch <= 0;
    pc <= startpc;
    stat <= AOK;
  endmethod
endmodule
