/*
 * Generated by Bluespec Compiler, version 2012.01.A (build 26572, 2012-01-17)
 * 
 * On Thu Apr 23 21:38:41 KST 2015
 * 
 */

/* Generation options: */
#ifndef __mkCop_h__
#define __mkCop_h__

#include "bluesim_types.h"
#include "bs_module.h"
#include "bluesim_primitives.h"
#include "bs_vcd.h"

namespace bluesim
{
  
  /* Class declaration for the mkCop module */
  class MOD_mkCop : public Module {
   
   /* Clock handles */
   private:
    tClock __clk_handle_0;
   
   /* Clock gate handles */
   public:
    tUInt8 *clk_gate[0];
   
   /* Instantiation parameters */
   public:
   
   /* Module state */
   public:
    MOD_Reg<tUWide> INST_copFifo_data;
    MOD_Reg<tUWide> INST_copFifo_data_1;
    MOD_Wire<tUInt8> INST_copFifo_deqEn_dummy;
    MOD_Reg<tUInt8> INST_copFifo_deqEn_dummy2;
    MOD_Reg<tUInt8> INST_copFifo_deqEn_dummy2_1;
    MOD_Reg<tUInt8> INST_copFifo_deqEn_dummy2_2;
    MOD_Wire<tUInt8> INST_copFifo_deqEn_dummy_1;
    MOD_Wire<tUInt8> INST_copFifo_deqEn_dummy_2;
    MOD_Wire<tUInt8> INST_copFifo_deqEn_dummy_3;
    MOD_Wire<tUInt8> INST_copFifo_deqEn_dummy_4;
    MOD_Wire<tUInt8> INST_copFifo_deqEn_dummy_5;
    MOD_Wire<tUInt8> INST_copFifo_deqEn_dummy_6;
    MOD_Wire<tUInt8> INST_copFifo_deqEn_dummy_7;
    MOD_Wire<tUInt8> INST_copFifo_deqEn_dummy_8;
    MOD_Wire<tUInt8> INST_copFifo_deqEn_lat;
    MOD_Wire<tUInt8> INST_copFifo_deqEn_lat_1;
    MOD_Wire<tUInt8> INST_copFifo_deqEn_lat_2;
    MOD_Reg<tUInt8> INST_copFifo_deqEn_rl;
    MOD_Wire<tUInt8> INST_copFifo_deqP_dummy;
    MOD_Reg<tUInt8> INST_copFifo_deqP_dummy2;
    MOD_Reg<tUInt8> INST_copFifo_deqP_dummy2_1;
    MOD_Reg<tUInt8> INST_copFifo_deqP_dummy2_2;
    MOD_Wire<tUInt8> INST_copFifo_deqP_dummy_1;
    MOD_Wire<tUInt8> INST_copFifo_deqP_dummy_2;
    MOD_Wire<tUInt8> INST_copFifo_deqP_dummy_3;
    MOD_Wire<tUInt8> INST_copFifo_deqP_dummy_4;
    MOD_Wire<tUInt8> INST_copFifo_deqP_dummy_5;
    MOD_Wire<tUInt8> INST_copFifo_deqP_dummy_6;
    MOD_Wire<tUInt8> INST_copFifo_deqP_dummy_7;
    MOD_Wire<tUInt8> INST_copFifo_deqP_dummy_8;
    MOD_Wire<tUInt8> INST_copFifo_deqP_lat;
    MOD_Wire<tUInt8> INST_copFifo_deqP_lat_1;
    MOD_Wire<tUInt8> INST_copFifo_deqP_lat_2;
    MOD_Reg<tUInt8> INST_copFifo_deqP_rl;
    MOD_Wire<tUInt8> INST_copFifo_enqEn_dummy;
    MOD_Reg<tUInt8> INST_copFifo_enqEn_dummy2;
    MOD_Reg<tUInt8> INST_copFifo_enqEn_dummy2_1;
    MOD_Reg<tUInt8> INST_copFifo_enqEn_dummy2_2;
    MOD_Wire<tUInt8> INST_copFifo_enqEn_dummy_1;
    MOD_Wire<tUInt8> INST_copFifo_enqEn_dummy_2;
    MOD_Wire<tUInt8> INST_copFifo_enqEn_dummy_3;
    MOD_Wire<tUInt8> INST_copFifo_enqEn_dummy_4;
    MOD_Wire<tUInt8> INST_copFifo_enqEn_dummy_5;
    MOD_Wire<tUInt8> INST_copFifo_enqEn_dummy_6;
    MOD_Wire<tUInt8> INST_copFifo_enqEn_dummy_7;
    MOD_Wire<tUInt8> INST_copFifo_enqEn_dummy_8;
    MOD_Wire<tUInt8> INST_copFifo_enqEn_lat;
    MOD_Wire<tUInt8> INST_copFifo_enqEn_lat_1;
    MOD_Wire<tUInt8> INST_copFifo_enqEn_lat_2;
    MOD_Reg<tUInt8> INST_copFifo_enqEn_rl;
    MOD_Wire<tUInt8> INST_copFifo_enqP_dummy;
    MOD_Reg<tUInt8> INST_copFifo_enqP_dummy2;
    MOD_Reg<tUInt8> INST_copFifo_enqP_dummy2_1;
    MOD_Reg<tUInt8> INST_copFifo_enqP_dummy2_2;
    MOD_Wire<tUInt8> INST_copFifo_enqP_dummy_1;
    MOD_Wire<tUInt8> INST_copFifo_enqP_dummy_2;
    MOD_Wire<tUInt8> INST_copFifo_enqP_dummy_3;
    MOD_Wire<tUInt8> INST_copFifo_enqP_dummy_4;
    MOD_Wire<tUInt8> INST_copFifo_enqP_dummy_5;
    MOD_Wire<tUInt8> INST_copFifo_enqP_dummy_6;
    MOD_Wire<tUInt8> INST_copFifo_enqP_dummy_7;
    MOD_Wire<tUInt8> INST_copFifo_enqP_dummy_8;
    MOD_Wire<tUInt8> INST_copFifo_enqP_lat;
    MOD_Wire<tUInt8> INST_copFifo_enqP_lat_1;
    MOD_Wire<tUInt8> INST_copFifo_enqP_lat_2;
    MOD_Reg<tUInt8> INST_copFifo_enqP_rl;
    MOD_Wire<tUWide> INST_copFifo_tempData_dummy;
    MOD_Reg<tUInt8> INST_copFifo_tempData_dummy2;
    MOD_Reg<tUInt8> INST_copFifo_tempData_dummy2_1;
    MOD_Wire<tUWide> INST_copFifo_tempData_dummy_1;
    MOD_Wire<tUWide> INST_copFifo_tempData_dummy_2;
    MOD_Wire<tUWide> INST_copFifo_tempData_dummy_3;
    MOD_Wire<tUWide> INST_copFifo_tempData_lat;
    MOD_Wire<tUWide> INST_copFifo_tempData_lat_1;
    MOD_Reg<tUWide> INST_copFifo_tempData_rl;
    MOD_Wire<tUInt8> INST_copFifo_tempEnqP_dummy;
    MOD_Reg<tUInt8> INST_copFifo_tempEnqP_dummy2;
    MOD_Reg<tUInt8> INST_copFifo_tempEnqP_dummy2_1;
    MOD_Wire<tUInt8> INST_copFifo_tempEnqP_dummy_1;
    MOD_Wire<tUInt8> INST_copFifo_tempEnqP_dummy_2;
    MOD_Wire<tUInt8> INST_copFifo_tempEnqP_dummy_3;
    MOD_Wire<tUInt8> INST_copFifo_tempEnqP_lat;
    MOD_Wire<tUInt8> INST_copFifo_tempEnqP_lat_1;
    MOD_Reg<tUInt8> INST_copFifo_tempEnqP_rl;
    MOD_Reg<tUInt32> INST_cycles;
    MOD_ConfigReg<tUInt32> INST_finishCode;
    MOD_ConfigReg<tUInt8> INST_finishReg;
    MOD_ConfigReg<tUInt32> INST_numBPMiss;
    MOD_ConfigReg<tUInt32> INST_numCtr;
    MOD_ConfigReg<tUInt32> INST_numInsts;
    MOD_ConfigReg<tUInt32> INST_numMem;
    MOD_ConfigReg<tUInt8> INST_startReg;
    MOD_ConfigReg<tUInt32> INST_timeReg;
   
   /* Constructor */
   public:
    MOD_mkCop(char const *name, Module *parent);
   
   /* Symbol init methods */
   private:
    void init_symbols_0();
   
   /* Reset signal definitions */
   private:
    tUInt8 PORT_RST_N;
   
   /* Port definitions */
   public:
    tUWide PORT_cpuToHost;
   
   /* Publicly accessible definitions */
   public:
    tUInt8 DEF_copFifo_deqEn_rl__h6934;
    tUInt8 DEF_copFifo_enqEn_rl__h5333;
    tUInt8 DEF_copFifo_deqEn_dummy2_2__h11048;
    tUInt8 DEF_copFifo_enqEn_dummy2_2__h9545;
   
   /* Local definitions */
   private:
    tUInt8 DEF_IF_copFifo_tempEnqP_lat_whas__1_THEN_copFifo_t_ETC___d190;
    tUInt8 DEF_IF_copFifo_tempEnqP_lat_whas__1_THEN_NOT_copFi_ETC___d192;
    tUInt8 DEF_copFifo_deqEn_lat_1_whas____d213;
    tUInt8 DEF_copFifo_enqEn_lat_1_whas____d211;
    tUWide DEF_ab__h8052;
    tUWide DEF_ab__h8006;
    tUWide DEF_ab__h8048;
    tUWide DEF_ab__h15888;
    tUWide DEF_ab__h15884;
    tUInt32 DEF__read__h12828;
    tUInt32 DEF_x__h15230;
    tUInt8 DEF_copFifo_tempEnqP_rl___d208;
    tUInt8 DEF_copFifo_tempEnqP_lat_wget____d209;
    tUInt8 DEF_upd__h15542;
    tUInt8 DEF_upd__h10168;
    tUInt8 DEF_upd__h10201;
    tUInt8 DEF_upd__h13602;
    tUInt8 DEF_upd__h9888;
    tUInt8 DEF_upd__h9921;
    tUInt8 DEF_copFifo_tempEnqP_lat_whas____d216;
    tUInt8 DEF_copFifo_deqEn_lat_1_wget____d206;
    tUInt8 DEF_copFifo_deqEn_lat_whas____d214;
    tUInt8 DEF_copFifo_deqEn_lat_wget____d205;
    tUInt8 DEF_copFifo_enqEn_lat_1_wget____d204;
    tUInt8 DEF_copFifo_enqEn_lat_whas____d212;
    tUInt8 DEF_copFifo_enqEn_lat_wget____d203;
    tUInt8 DEF_copFifo_deqP_dummy2_2__h10129;
    tUInt8 DEF_copFifo_enqP_dummy2_2__h9849;
    tUInt8 DEF_copFifo_tempEnqP_lat_wget__2_BITS_2_TO_0___d202;
    tUInt8 DEF_copFifo_tempEnqP_rl_4_BIT_3___d218;
    tUInt8 DEF_copFifo_tempEnqP_lat_wget__2_BIT_3___d217;
    tUWide DEF_IF_copFifo_tempData_dummy2_1_19_THEN_IF_copFif_ETC___d223;
    tUWide DEF_IF_copFifo_tempData_lat_whas__3_THEN_copFifo_t_ETC___d189;
    tUWide DEF_IF_copFifo_tempData_lat_1_whas__1_THEN_copFifo_ETC___d47;
    tUInt8 DEF_IF_copFifo_tempEnqP_lat_whas__1_THEN_copFifo_t_ETC___d201;
    tUInt8 DEF_IF_copFifo_deqP_lat_1_whas__3_THEN_copFifo_deq_ETC___d199;
    tUInt8 DEF_IF_copFifo_enqP_lat_1_whas_THEN_copFifo_enqP_l_ETC___d197;
    tUWide DEF_IF_IF_wr_idx_BIT_5_36_THEN_wr_idx_BITS_3_TO_0__ETC___d150;
   
   /* Rules */
   public:
    void RL_copFifo_enqP_canon();
    void RL_copFifo_deqP_canon();
    void RL_copFifo_enqEn_canon();
    void RL_copFifo_deqEn_canon();
    void RL_copFifo_tempData_canon();
    void RL_copFifo_tempEnqP_canon();
    void RL_copFifo_canonicalize();
    void RL_count();
   
   /* Methods */
   public:
    void METH_start();
    tUInt8 METH_RDY_start();
    tUInt8 METH_started();
    tUInt8 METH_RDY_started();
    tUInt32 METH_rd(tUInt8 ARG_rd_idx);
    tUInt8 METH_RDY_rd();
    void METH_wr(tUInt8 ARG_wr_idx, tUInt32 ARG_wr_val);
    tUInt8 METH_RDY_wr();
    void METH_incInstTypeCnt(tUInt8 ARG_incInstTypeCnt_idx);
    tUInt8 METH_RDY_incInstTypeCnt();
    void METH_incBPMissCnt();
    tUInt8 METH_RDY_incBPMissCnt();
    tUWide METH_cpuToHost();
    tUInt8 METH_RDY_cpuToHost();
   
   /* Reset routines */
   public:
    void reset_RST_N(tUInt8 ARG_rst_in);
   
   /* Static handles to reset routines */
   public:
   
   /* Pointers to reset fns in parent module for asserting output resets */
   private:
   
   /* Functions for the parent module to register its reset fns */
   public:
   
   /* Functions to set the elaborated clock id */
   public:
    void set_clk_0(char const *s);
   
   /* State dumping routine */
   public:
    void dump_state(unsigned int indent);
   
   /* VCD dumping routines */
   public:
    unsigned int dump_VCD_defs(unsigned int levels);
    void dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkCop &backing);
    void vcd_defs(tVCDDumpType dt, MOD_mkCop &backing);
    void vcd_prims(tVCDDumpType dt, MOD_mkCop &backing);
  };
}

#endif /* ifndef __mkCop_h__ */
