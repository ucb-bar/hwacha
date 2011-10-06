`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"

module disasm_cmd
(
  input [`XCMD_CMD_SZ-1:0] cmd
);

`ifndef SYNTHESIS
  reg [71:0] cmdasm;

  always @(*)
  begin
    casez (cmd)
      `CMD_VVCFGIVL : $sformat(cmdasm, "vvcfgivl");
      `CMD_VSETVL   : $sformat(cmdasm, "vsetvl");
      `CMD_VF       : $sformat(cmdasm, "vf");

      `CMD_FENCE_L_V : $sformat(cmdasm, "sync.l.v");
      `CMD_FENCE_G_V : $sformat(cmdasm, "sync.g.v");
      `CMD_FENCE_L_CV: $sformat(cmdasm, "sync.l.cv");
      `CMD_FENCE_G_CV: $sformat(cmdasm, "sync.g.cv");

      `CMD_LDWB     : $sformat(cmdasm, "ldwb");
      `CMD_STAC     : $sformat(cmdasm, "stac");

      `CMD_VMVV     : $sformat(cmdasm, "vmvv");
      `CMD_VMSV     : $sformat(cmdasm, "vmsv");
      `CMD_VMST     : $sformat(cmdasm, "vmst");
      `CMD_VMTS     : $sformat(cmdasm, "vmts");
      `CMD_VFMVV    : $sformat(cmdasm, "vfmvv");
      `CMD_VFMSV    : $sformat(cmdasm, "vfmsv");
      `CMD_VFMST    : $sformat(cmdasm, "vfmst");
      `CMD_VFMTS    : $sformat(cmdasm, "vfmts");

      `CMD_VLD      : $sformat(cmdasm, "vld");
      `CMD_VLW      : $sformat(cmdasm, "vlw");
      `CMD_VLWU     : $sformat(cmdasm, "vlwu");
      `CMD_VLH      : $sformat(cmdasm, "vlh");
      `CMD_VLHU     : $sformat(cmdasm, "vlhu");
      `CMD_VLB      : $sformat(cmdasm, "vlb");
      `CMD_VLBU     : $sformat(cmdasm, "vlbu");
      `CMD_VSD      : $sformat(cmdasm, "vsd");
      `CMD_VSW      : $sformat(cmdasm, "vsw");
      `CMD_VSH      : $sformat(cmdasm, "vsh");
      `CMD_VSB      : $sformat(cmdasm, "vsb");

      `CMD_VFLD     : $sformat(cmdasm, "vfld");
      `CMD_VFLW     : $sformat(cmdasm, "vflw");
      `CMD_VFSD     : $sformat(cmdasm, "vfsd");
      `CMD_VFSW     : $sformat(cmdasm, "vfsw");

      `CMD_VLSTD    : $sformat(cmdasm, "vlstd");
      `CMD_VLSTW    : $sformat(cmdasm, "vlstw");
      `CMD_VLSTWU   : $sformat(cmdasm, "vlstwu");
      `CMD_VLSTH    : $sformat(cmdasm, "vlsth");
      `CMD_VLSTHU   : $sformat(cmdasm, "vlsthu");
      `CMD_VLSTB    : $sformat(cmdasm, "vlstb");
      `CMD_VLSTBU   : $sformat(cmdasm, "vlstbu");
      `CMD_VSSTD    : $sformat(cmdasm, "vsstd");
      `CMD_VSSTW    : $sformat(cmdasm, "vsstw");
      `CMD_VSSTH    : $sformat(cmdasm, "vssth");
      `CMD_VSSTB    : $sformat(cmdasm, "vsstb");

      `CMD_VFLSTD   : $sformat(cmdasm, "vflstd");
      `CMD_VFLSTW   : $sformat(cmdasm, "vflstw");
      `CMD_VFSSTD   : $sformat(cmdasm, "vfsstd");
      `CMD_VFSSTW   : $sformat(cmdasm, "vfsstw");

      `CMD_VLXD     : $sformat(cmdasm, "vlxd");
      `CMD_VLXW     : $sformat(cmdasm, "vlxw");
      `CMD_VLXWU    : $sformat(cmdasm, "vlxwu");
      `CMD_VLXH     : $sformat(cmdasm, "vlxh");
      `CMD_VLXHU    : $sformat(cmdasm, "vlxhu");
      `CMD_VLXB     : $sformat(cmdasm, "vlxb");
      `CMD_VLXBU    : $sformat(cmdasm, "vlxbu");
      `CMD_VSXD     : $sformat(cmdasm, "vsxd");
      `CMD_VSXW     : $sformat(cmdasm, "vsxw");
      `CMD_VSXH     : $sformat(cmdasm, "vsxh");
      `CMD_VSXB     : $sformat(cmdasm, "vsxb");

      `CMD_VFLXD    : $sformat(cmdasm, "vflxd");
      `CMD_VFLXW    : $sformat(cmdasm, "vflxw");
      `CMD_VFSXD    : $sformat(cmdasm, "vfsxd");
      `CMD_VFSXW    : $sformat(cmdasm, "vfsxw");

      default       : $sformat(cmdasm, "undef");
    endcase
  end

`endif

endmodule
