     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/title ('Retrieve the IP address of the Current Device')
      *==========================================================
      * To Compile:
      *
      *    CRTBNDRPG  PGM(xxx/RTVIPR)
      *
      * Function: Retrieve IP address of PC
      *
      *==========================================================
     H*  Revisions:                                                       *
     H*                                                                   *
     H*  12/07/12  C:33  Project: SY266                                   *
     H*                  Use an external procedure to get IP address.     *
     H*                                                                   *
     H*  10/08/08  C:33  Project: DR214  ID099/60991                      *
     H*                  Change offset on subset to get IP address.       *
      *===================================================================
     H DFTACTGRP(*NO) BNDDIR('QC2LE')
     D RtvIpAdr        PR            20a
     D  Device                       10a   CONST

     D GetEnv          PR              *   ExtProc('getenv')
     D   name                          *   Value Options(*String)
      *
     D I_Net_Adr       S             16a
      *
     D                SDS
     D  Device               244    253
     ***--------------------------------------------------------
     c     *ENTRY        PLIST
     c                   parm                    inip             16
      *---------------------------------------------------------
      * Retrieve IP Address
     C                   Eval      I_Net_Adr   = RtvIpAdr(Device)
     C                   Eval      I_Net_Adr   = %trim(I_Net_Adr)

     c                   Eval      inip = I_Net_Adr
      /Free
       If inIP = *blanks;
          inIP = %str(getenv('REMOTE_ADDR'));
       Endif;
      /End-free
     C**** I_Net_Adr     Dsply
     C                   Eval      *Inlr = *On
      *---------------------------------------------------------
      *    RtvIpAdr - Subprocedure To Retrieve PC's IP Address
      *---------------------------------------------------------
     P RtvIpAdr        B                   Export
     D RtvIpAdr        PI            20A
     D  Inp_Device                   10A   Const
     D Apierr          DS
     D  Bytprv                 1      4B 0 Inz(216)
     D  Bytavl                 5      8B 0 Inz
     D  Errid                  9     15A   Inz
     D  Rsvd                  16     16A   Inz
     D  Errdta                17    216A   Inz
     D Net_Address     S             20A   INZ
     D Format          S              8A   Inz('DEVD0600')
     D Rcvar           S           5000A   Inz
     D Varlen          S              4B 0 Inz(5000)
     C                   Eval      Device = Inp_Device
     C                   Call      'QDCRDEVD'
     C                   Parm                    Rcvar
     C                   Parm                    Varlen
     C                   Parm                    Format
     C                   Parm                    Device
     C                   Parm                    Apierr
     C                   If        BytAvl = 0
     C                   Eval      Net_Address = %Subst(Rcvar:878:15)
     C                   Endif
     C                   Return    Net_Address
     P RtvIpAdr        E
