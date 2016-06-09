/*
 * Generated by Bluespec Compiler, version 2012.01.A (build 26572, 2012-01-17)
 * 
 * On Thu Apr 23 21:38:41 KST 2015
 * 
 */
#include "bluesim_primitives.h"
#include "mkTestBench.h"

namespace bluesim
{
  
  /* String declarations */
  static std::string const __str_literal_1("\ncycle %d", 9u);
  static std::string const __str_literal_3("%c", 2u);
  static std::string const __str_literal_2("%d", 2u);
  static std::string const __str_literal_4("==================================\n", 35u);
  static std::string const __str_literal_6("Executed Instructions : %d\n", 27u);
  static std::string const __str_literal_5("Number of Cycles      : %d\n", 27u);
  static std::string const __str_literal_8("Result                :     FAILED %d\n", 38u);
  static std::string const __str_literal_7("Result                :     PASSED\n", 35u);
  
  
  /* Constructor */
  MOD_mkTestBench::MOD_mkTestBench(char const *name, Module *parent)
    : Module(name, parent),
      __clk_handle_0(BAD_CLOCK_HANDLE),
      INST_cycle("cycle", this, 32u, 0u, (tUInt8)0u),
      INST_proc("proc", this),
      INST_tState("tState", this, 1u, (tUInt8)0u, (tUInt8)0u),
      PORT_RST_N((tUInt8)1u),
      DEF_ab__h311(68u)
  {
    symbol_count = 6u;
    symbols = new tSym[symbol_count];
    init_symbols_0();
  }
  
  
  /* Symbol init fns */
  
  void MOD_mkTestBench::init_symbols_0()
  {
    init_symbol(&symbols[0u], "cycle", SYM_MODULE, &INST_cycle);
    init_symbol(&symbols[1u], "proc", SYM_MODULE, &INST_proc);
    init_symbol(&symbols[2u], "RL_countCycle", SYM_RULE);
    init_symbol(&symbols[3u], "RL_run", SYM_RULE);
    init_symbol(&symbols[4u], "RL_start", SYM_RULE);
    init_symbol(&symbols[5u], "tState", SYM_MODULE, &INST_tState);
  }
  
  
  /* Rule actions */
  
  void MOD_mkTestBench::RL_start()
  {
    INST_proc.METH_hostToCpu(0u);
    INST_tState.METH_write((tUInt8)1u);
  }
  
  void MOD_mkTestBench::RL_countCycle()
  {
    tUInt32 DEF_x__h266;
    DEF__read__h112 = INST_cycle.METH_read();
    DEF_x__h266 = DEF__read__h112 + 1u;
    INST_cycle.METH_write(DEF_x__h266);
    if (!(PORT_RST_N == (tUInt8)0u))
      dollar_display(this, "s,32", &__str_literal_1, DEF__read__h112);
  }
  
  void MOD_mkTestBench::RL_run()
  {
    tUInt8 DEF_proc_cpuToHost_BITS_67_TO_64_0_EQ_14_4_AND_pro_ETC___d17;
    tUInt8 DEF_proc_cpuToHost_BITS_67_TO_64_0_EQ_14_4_AND_NOT_ETC___d19;
    tUInt8 DEF_proc_cpuToHost_BITS_67_TO_64_0_EQ_12___d11;
    tUInt8 DEF_proc_cpuToHost_BITS_67_TO_64_0_EQ_13___d13;
    tUInt8 DEF_proc_cpuToHost_BITS_67_TO_64_0_EQ_14___d14;
    tUInt8 DEF_proc_cpuToHost_BITS_63_TO_32_2_EQ_0___d16;
    tUInt8 DEF_idx__h318;
    tUInt32 DEF_proc_cpuToHost_BITS_31_TO_0___d15;
    tUInt32 DEF_data__h320;
    tUWide DEF_AVMeth_proc_cpuToHost(68u, false);
    DEF__read__h112 = INST_cycle.METH_read();
    DEF_AVMeth_proc_cpuToHost = INST_proc.METH_cpuToHost();
    DEF_ab__h311 = DEF_AVMeth_proc_cpuToHost;
    DEF_data__h320 = primExtract32(32u, 68u, DEF_ab__h311, 32u, 63u, 32u, 32u);
    DEF_proc_cpuToHost_BITS_31_TO_0___d15 = primExtract32(32u, 68u, DEF_ab__h311, 32u, 31u, 32u, 0u);
    DEF_idx__h318 = primExtract8(4u, 68u, DEF_ab__h311, 32u, 67u, 32u, 64u);
    DEF_proc_cpuToHost_BITS_63_TO_32_2_EQ_0___d16 = DEF_data__h320 == 0u;
    DEF_proc_cpuToHost_BITS_67_TO_64_0_EQ_14___d14 = DEF_idx__h318 == (tUInt8)14u;
    DEF_proc_cpuToHost_BITS_67_TO_64_0_EQ_13___d13 = DEF_idx__h318 == (tUInt8)13u;
    DEF_proc_cpuToHost_BITS_67_TO_64_0_EQ_12___d11 = DEF_idx__h318 == (tUInt8)12u;
    DEF_proc_cpuToHost_BITS_67_TO_64_0_EQ_14_4_AND_NOT_ETC___d19 = DEF_proc_cpuToHost_BITS_67_TO_64_0_EQ_14___d14 && !DEF_proc_cpuToHost_BITS_63_TO_32_2_EQ_0___d16;
    DEF_proc_cpuToHost_BITS_67_TO_64_0_EQ_14_4_AND_pro_ETC___d17 = DEF_proc_cpuToHost_BITS_67_TO_64_0_EQ_14___d14 && DEF_proc_cpuToHost_BITS_63_TO_32_2_EQ_0___d16;
    if (!(PORT_RST_N == (tUInt8)0u))
    {
      if (DEF_proc_cpuToHost_BITS_67_TO_64_0_EQ_12___d11)
	dollar_fwrite(this, "32,s,32", 2147483650u, &__str_literal_2, DEF_data__h320);
      if (DEF_proc_cpuToHost_BITS_67_TO_64_0_EQ_13___d13)
	dollar_fwrite(this, "32,s,32", 2147483650u, &__str_literal_3, DEF_data__h320);
      if (DEF_proc_cpuToHost_BITS_67_TO_64_0_EQ_14___d14)
	dollar_fwrite(this, "32,s", 2147483650u, &__str_literal_4);
      if (DEF_proc_cpuToHost_BITS_67_TO_64_0_EQ_14___d14)
	dollar_fwrite(this, "32,s,32", 2147483650u, &__str_literal_5, DEF__read__h112);
      if (DEF_proc_cpuToHost_BITS_67_TO_64_0_EQ_14___d14)
	dollar_fwrite(this,
		      "32,s,32",
		      2147483650u,
		      &__str_literal_6,
		      DEF_proc_cpuToHost_BITS_31_TO_0___d15);
      if (DEF_proc_cpuToHost_BITS_67_TO_64_0_EQ_14_4_AND_pro_ETC___d17)
	dollar_fwrite(this, "32,s", 2147483650u, &__str_literal_7);
      if (DEF_proc_cpuToHost_BITS_67_TO_64_0_EQ_14_4_AND_NOT_ETC___d19)
	dollar_fwrite(this, "32,s,32", 2147483650u, &__str_literal_8, DEF_data__h320);
      if (DEF_proc_cpuToHost_BITS_67_TO_64_0_EQ_14___d14)
	dollar_fwrite(this, "32,s", 2147483650u, &__str_literal_4);
      if (DEF_proc_cpuToHost_BITS_67_TO_64_0_EQ_14___d14)
	dollar_finish("32", 1u);
    }
  }
  
  
  /* Methods */
  
  
  /* Reset routines */
  
  void MOD_mkTestBench::reset_RST_N(tUInt8 ARG_rst_in)
  {
    PORT_RST_N = ARG_rst_in;
    INST_tState.reset_RST_N(ARG_rst_in);
    INST_proc.reset_RST_N(ARG_rst_in);
    INST_cycle.reset_RST_N(ARG_rst_in);
  }
  
  
  /* Static handles to reset routines */
  
  
  /* Functions for the parent module to register its reset fns */
  
  
  /* Functions to set the elaborated clock id */
  
  void MOD_mkTestBench::set_clk_0(char const *s)
  {
    __clk_handle_0 = bk_get_or_define_clock(s);
  }
  
  
  /* State dumping routine */
  void MOD_mkTestBench::dump_state(unsigned int indent)
  {
    printf("%*s%s:\n", indent, "", inst_name);
    INST_cycle.dump_state(indent + 2u);
    INST_proc.dump_state(indent + 2u);
    INST_tState.dump_state(indent + 2u);
  }
  
  
  /* VCD dumping routines */
  
  unsigned int MOD_mkTestBench::dump_VCD_defs(unsigned int levels)
  {
    fprintf(vcd_file, "$scope module %s $end\n", inst_name);
    vcd_num = vcd_reserve_ids(5u);
    unsigned int num = vcd_num;
    for (unsigned int hdl = 0u; hdl < bk_num_clocks(); ++hdl)
      vcd_add_clock_def(this, bk_clock_name(hdl), bk_clock_vcd_num(hdl));
    vcd_write_def(bk_clock_vcd_num(__clk_handle_0), "CLK", 1u);
    vcd_write_def(num++, "RST_N", 1u);
    vcd_set_clock(num, __clk_handle_0);
    vcd_write_def(num++, "_read__h112", 32u);
    vcd_set_clock(num, __clk_handle_0);
    vcd_write_def(num++, "ab__h311", 68u);
    num = INST_cycle.dump_VCD_defs(num);
    num = INST_tState.dump_VCD_defs(num);
    if (levels != 1u)
    {
      unsigned int l = levels == 0u ? 0u : levels - 1u;
      num = INST_proc.dump_VCD_defs(l);
    }
    fprintf(vcd_file, "$upscope $end\n");
    return num;
  }
  
  void MOD_mkTestBench::dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkTestBench &backing)
  {
    vcd_defs(dt, backing);
    vcd_prims(dt, backing);
    if (levels != 1u)
      vcd_submodules(dt, levels - 1u, backing);
  }
  
  void MOD_mkTestBench::vcd_defs(tVCDDumpType dt, MOD_mkTestBench &backing)
  {
    unsigned int num = vcd_num;
    if (dt == VCD_DUMP_XS)
    {
      vcd_write_x(num++, 1u);
      vcd_write_x(num++, 32u);
      vcd_write_x(num++, 68u);
    }
    else
      if (dt == VCD_DUMP_CHANGES)
      {
	if ((backing.PORT_RST_N) != PORT_RST_N)
	{
	  vcd_write_val(num, PORT_RST_N, 1u);
	  backing.PORT_RST_N = PORT_RST_N;
	}
	++num;
	if ((backing.DEF__read__h112) != DEF__read__h112)
	{
	  vcd_write_val(num, DEF__read__h112, 32u);
	  backing.DEF__read__h112 = DEF__read__h112;
	}
	++num;
	if ((backing.DEF_ab__h311) != DEF_ab__h311)
	{
	  vcd_write_val(num, DEF_ab__h311, 68u);
	  backing.DEF_ab__h311 = DEF_ab__h311;
	}
	++num;
      }
      else
      {
	vcd_write_val(num++, PORT_RST_N, 1u);
	backing.PORT_RST_N = PORT_RST_N;
	vcd_write_val(num++, DEF__read__h112, 32u);
	backing.DEF__read__h112 = DEF__read__h112;
	vcd_write_val(num++, DEF_ab__h311, 68u);
	backing.DEF_ab__h311 = DEF_ab__h311;
      }
  }
  
  void MOD_mkTestBench::vcd_prims(tVCDDumpType dt, MOD_mkTestBench &backing)
  {
    INST_cycle.dump_VCD(dt, backing.INST_cycle);
    INST_tState.dump_VCD(dt, backing.INST_tState);
  }
  
  void MOD_mkTestBench::vcd_submodules(tVCDDumpType dt, unsigned int levels, MOD_mkTestBench &backing)
  {
    INST_proc.dump_VCD(dt, levels, backing.INST_proc);
  }
}
