     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*NEW)
     H*****************************************************************
     H/TITLE W0051R63 - Maintain Fee Charges
     H*********************************************************************
     H*  DESCRIPTION:  This program will allow a user to change, view     *
     H*                or add records in the FEES CHARGES.                *
     H*                                                                   *
     H*  CALLED FROM:                                                     *
     H*    W0051R63 - FEES CHARGES MAINTENANCE                            *
     H*  CALLS TO:                                                        *
     H*    NONE                                                           *
     H*                                                                   *
     H*  CREATED: 01/25/10  PROGRAMMER: C:232 PROJECT: WC1851             *
     H*********************************************************************
     H*  REVISIONS:                                                       *
     H*                                                                   *
     H* 09/21/11  C:65  Project: WC1851                                   *
     H*                 Change activation group above to *NEW.            *
     H*                                                                   *
     H* 08/19/11 C:65  Project: WC1851                                    *
     H*                Use state and effective date to chain to proper    *
     H*                WTFIP record prior to writing the WDEFP record.    *
     H*                                                                   *
     H* 05/06/11  C:65  Project: WC1851                                   *
     H*                 Write WDEFP records when called in Add mode from  *
     H*                 employer detail.  Write one record for each       *
     H*                 fee detail setup using all default values.        *
     H*                                                                   *
     H* 02/11/11  C:33  Project: WC1851                                   *
     H*                 Re-write.  No comments.                           *
     H*                                                                   *
     H* 02/24/09  C:33  Project: WC1851   25WQL Priss# 12                 *
     H*                 Re-write.  No comments.                           *
     H*********************************************************************
     FW0051F63  CF   E             WORKSTN INFDS(FIDS01)
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FWDEFP     UF A E             DISK    INFDS(DBFDS)
     F                                     RENAME(PEF01:DBRCDF)
     FWDEFL1    uf a e           k disk    INFDS(effds)
     FWDELP     IF   E             DISK    rename(pel01:pel0p)
     Fwdell1    IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWTFIP     IF   E             DISK    rename(pfi01:pfi0p)
     FWTFIL     IF   E           K DISK    rename(pfi01:pfi00)
     FWMRSL     IF   E           K DISK
     FWTABL     IF   E           K DISK    ignore(prs01:ppa01)
      *
     Fwtgtl     if   e           k disk    rename(pgt01:pgt00)
     FWMFNL     IF   E           K DISK
     Fwmexl     if   e           k disk    rename(pex01:pex00)
     FWDF1L     IF   E           K DISK
     f*** begin add ***  08/19/11  *************************************************
     FWTFTL     IF   E           K DISK
     f*** end add *****  08/19/11  *************************************************
      *
     d revised         s              1n
     d holdDiv         s                   like(ELDIV)
     d holdSt          s                   like(RSJRST)
     d xxSt            s                   like(RSJRST)
     d emRRN           s              9  0
     d ##kc            s              1n
     d ##kl            s              1n
     d ##RRN           s              9  0
     D getOut          s              1n   inz
     d addMode         s              1n
     d cpyMode         s              1n
     d dspMode         s              1n
     d updMode         s              1n
     d sysDt7          s                   like(EFDT)
     D*================================================================
     D*  Data Structures & constants definitions
     D*================================================================
     DkeyWDEFL1        ds                  likerec(pef01:*key)    inz
     Din1WDEFL1        ds                  likerec(pef01:*input)
     Din2WDEFL1        ds                  likerec(pef01:*input)
     DoutWDEFL1        ds                  likerec(pef01:*output)
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
      *
     D effds           DS
     D  efrrn                397    400B 0
      *
     D OPTNM           C                   CONST('Maintain Fee Information')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D #OFSCR          C                   CONST(1)
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
    D*
     DGetEmpSt         pr                  like(RSJRST)
     D inCo#                               like(RSCO#)
     D inRtCd                              like(RSRTCD)
     D*
     DNextPgm          pr             1n
     D inKc                           1n
     D inKl                           1n
     D*
     DPgmExistsInStk   pr                  extPgm('SRCHKPGM')
     D program                       10                 const
     D exists                         1n
     D*
     DProcessDivs      pr             1n
     D*
     DW0063RB          pr                  extPgm('W0063RB')
     D outKc                          1n
     D outKl                          1n
     D outMode                        1    const
     D outRRN                         9  0
     D*
     D/copy wPrGroup
     D/copy wPrEmplr
     D*
     C/copy sCopyMT42c
      *
     C*================================================================
     C*  PLIST Definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  AGENT/PRODUCER RETRIEVE PROGRAM PARAMETER LIST
     C     AGTPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      ELFND         ##FND
     C                   PARM      ELEMP#        ##EMP#            9 0
     C                   PARM      ELDIV         ##DIV             5 0
     C                   PARM                    ##FYR             3 0
     C     SCAGT#        PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C     SCPROD        PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
     C                   PARM      *ZERO         ##AREA            3 0
     C                   PARM      *ZERO         ##PHON            7 0
     C                   PARM      '*'           ##PLAN            1
     C                   PARM      *ZERO         ##DATE            8 0
     C*
     C     nextPgmPl     plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##Mode
     C                   parm                    ##rrn
     C*
     C*================================================================
     C*  KLIST Definitions
     C*================================================================
     C*
     C     EMKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C     EMKEY         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C*
     C*  GROUP MASTER
     C*
     C     FNKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C*
     C*  KEY FOR EMPLOYER DETAIL FEE INFORMATION...
     C*
     C     EFKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C                   KFLD                    FITYPE
     C                   KFLD                    FIDESC
     C*
     C     RTKY          KLIST
     C                   KFLD                    ELCO#
     C*
     C* KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    XZTBID
     C                   KFLD                    XXKEY
     C*
     C*  GROUP DETAIL FILE
     C*
     C     F1KY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
      *
     C     RSKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
      *
     C     fiky          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     c*** begin add ***  08/19/11  *************************************************
     c                   kfld                    fttype
     c                   kfld                    ftdesc
     c*** end add *****  08/19/11  *************************************************
     C                   KFLD                    XXST
     C                   KFLD                    ELPFDT
      *
     C     fiky1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     c*** begin add ***  08/19/11  *************************************************
     c                   kfld                    fttype
     c                   kfld                    ftdesc
     c*** end add *****  08/19/11  *************************************************
     C                   KFLD                    XXST

     C*================================================================
     C*  *LIKE DEFN definitions
     C*================================================================
     C     *LIKE         DEFINE    @@mode        ##mode
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    GTTBID        XZTBID
     C     *LIKE         DEFINE    ELPTDT        XXDAT2
     C     *LIKE         DEFINE    ELPFDT        XXDAT1
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    EMCAD1        XXCXDT
     C*
    C*================================================================
    C* Delete record so it can be re-written, if needed
    C*================================================================
     C     DeleteR       begsr

     C     @@RRN         delete    dbrcdf                             99

     C                   endsr
     C*
     C*===============================================================
     C* DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
     C*===============================================================
     C     DLTPRE        begsr
     C                   endsr
     C*
     C*===============================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*===============================================================
     C     CHGSTS        begsr
     C                   endsr                                                  CHGSTS
     C*
     C*===============================================================
     C* DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
     C*===============================================================
     C     DSPLYR        begsr
     C                   endsr
     C*
     C*===============================================================
     C* ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
     C*===============================================================
     C     ERRCK1        begsr
      *
     C*
     C                   endsr                                                  ERRCK1
     C*
     C*===============================================================
     C* ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
     C*===============================================================
     C     ERRCK2        begsr
     C*
     C                   endsr                                                  ERRCK2
     C*
     C*===============================================================
     C* ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
     C*===============================================================
     C     ERRCK3        begsr
     C                   endsr                                                  ERRCK3
     C*
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C*  F10=TABLE SEARCH
     C*
     C     KJDO          begsr
     C                   MOVEA     '00000000'    *IN(25)
     C                   MOVEA     '00000000'    *IN(33)
     C                   MOVEA     '00000000'    *IN(40)
     C                   MOVEA     '00000000'    *IN(48)
     C***                  SETOF                     3742
     C*
     C                   if        DtCheck(scScBd) <> *zero
     C                   eval      scScBd = DtCheck(scScBd)
     C                   endif
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCROBT'
     C*
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM      '2'           QQCALL            1
     C                   PARM      'CFBB'        QQID              4
     C                   PARM      *BLANKS       QQKEY            10
     C                   PARM                    QQRTCD            1
     C*
     C     QQKEY         IFNE      *BLANKS
     C                   SETON                                        26
     C                   MOVEL     QQKEY         SCROBT
     C                   ELSE
     C                   SETON                                        25
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCMOBT'
     C*
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM      '2'           QQCALL            1
     C                   PARM      'CFBB'        QQID              4
     C                   PARM      *BLANKS       QQKEY            10
     C                   PARM                    QQRTCD            1
     C*
     C     QQKEY         IFNE      *BLANKS
     C                   SETON                                        29
     C                   MOVEL     QQKEY         SCMOBT
     C                   ELSE
     C                   SETON                                        28
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCINBT'
     C*
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM      '2'           QQCALL            1
     C                   PARM      'CFBB'        QQID              4
     C                   PARM      *BLANKS       QQKEY            10
     C                   PARM                    QQRTCD            1
     C*
     C     QQKEY         IFNE      *BLANKS
     C                   SETON                                        32
     C                   MOVEL     QQKEY         SCINBT
     C                   ELSE
     C                   SETON                                        31
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCALBT'
     C*
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM      '2'           QQCALL            1
     C                   PARM      'CFBB'        QQID              4
     C                   PARM      *BLANKS       QQKEY            10
     C                   PARM                    QQRTCD            1
     C*
     C     QQKEY         IFNE      *BLANKS
     C                   SETON                                        35
     C                   MOVEL     QQKEY         SCALBT
     C                   ELSE
     C                   SETON                                        34
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCCOBS'
     C*
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM      '2'           QQCALL            1
     C                   PARM      'COBB'        QQID              4
     C                   PARM      *BLANKS       QQKEY            10
     C                   PARM                    QQRTCD            1
     C*
     C     QQKEY         IFNE      *BLANKS
     C                   SETON                                        38
     C                   MOVEL     QQKEY         SCCOBS
     C                   ELSE
     C                   SETON                                        37
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   endsr                                                  KJDO
     C*
     C*===============================================================
     C* LoadKeyWDEFL1 - Load key fields for keyWDEFL1
     C*===============================================================
     C     LoadKeyWDEFL1 begsr
      /free
       eval keyWDEFL1.EFCO#  = ELCO# ;
       eval keyWDEFL1.EFFND  = ELFND ;
       eval keyWDEFL1.EFEMP# = ELEMP#;
       eval keyWDEFL1.EFDIV  = ELDIV ;
       eval keyWDEFL1.EFFYR  = ELFYR ;
       eval keyWDEFL1.EFTYPE = FITYPE;
       eval keyWDEFL1.EFDESC = FIDESC;
      /end-free
     C                   endsr                                                  KJDO
     C*
     C*===============================================================
     C* Load mode indicators
     C*===============================================================
     C     LoadModeInd   begsr
     C*
     C                   eval      addMode = @@Mode = 'A'
     C                   eval      cpyMode = @@Mode = 'P'
     C                   eval      dspMode = @@Mode = 'D'
     C                   eval      updMode = @@Mode = 'C'
     C*
     C                   eval      *in09 = @@Mode = 'D'
     C*
     C                   endsr                                                  KJDO
     C*
     C*===============================================================
     C* LoadOutWDEFL1 - Load key fields for outWDEFL1
     C*===============================================================
     C     LoadOutWDEFL1 begsr
      /free
       // Key values;
       eval outWDEFL1.EFCO#  = ELCO# ;
       eval outWDEFL1.EFFND  = ELFND ;
       eval outWDEFL1.EFEMP# = ELEMP#;
       eval outWDEFL1.EFDIV  = ELDIV ;
       eval outWDEFL1.EFFYR  = ELFYR ;
       eval outWDEFL1.EFTYPE = FITYPE;
       eval outWDEFL1.EFDESC = FIDESC;
       // *** begin add ***  05/06/11  **********************************************
       // Set group values in file if override = 'Y'         (restored eval statements)
       if FIAOEL = 'Y';
       // *** end add *****  05/06/11  **********************************************
       // Claims fee;
       eval outWDEFL1.EFROPN = FIROPN;
       eval outWDEFL1.EFROFF = FIROFF;
       eval outWDEFL1.EFMOBT = FIMOBT;
       eval outWDEFL1.EFMOPN = FIMOPN;
       eval outWDEFL1.EFMOFF = FIMOFF;
       eval outWDEFL1.EFINBT = FIINBT;
       eval outWDEFL1.EFINPN = FIINPN;
       eval outWDEFL1.EFINFF = FIINFF;
       eval outWDEFL1.EFALBT = FIALBT;
       eval outWDEFL1.EFALPN = FIALPN;
       eval outWDEFL1.EFALFF = FIALFF;
       // *** begin add ***  05/06/11  **********************************************
       else;
       eval outWDEFL1.EFROPN = 0;
       eval outWDEFL1.EFROFF = 0;
       eval outWDEFL1.EFMOBT = ' ';
       eval outWDEFL1.EFMOPN = 0;
       eval outWDEFL1.EFMOFF = 0;
       eval outWDEFL1.EFINBT = ' ';
       eval outWDEFL1.EFINPN = 0;
       eval outWDEFL1.EFINFF = 0;
       eval outWDEFL1.EFALBT = ' ';
       eval outWDEFL1.EFALPN = 0;
       eval outWDEFL1.EFALFF = 0;
       endif;
       if FICAOE = 'Y';
       // *** end add *****  05/06/11  **********************************************
       // General fee;
       eval outWDEFL1.EFCOBS = FICOBB;
       eval outWDEFL1.EFCOMA = FIMNCA;
       eval outWDEFL1.EFCOPN = FICOPN;
       eval outWDEFL1.EFCOFF = FICOFF;
       // *** begin add ***  05/06/11  **********************************************
       else;
       eval outWDEFL1.EFCOBS = ' ';
       eval outWDEFL1.EFCOMA = 0;
       eval outWDEFL1.EFCOPN = 0;
       eval outWDEFL1.EFCOFF = 0;
       endif;
       eval outWDEFL1.EFSCBD = 0;
       // *** end add *****  05/06/11  **********************************************
      /end-free
     C                   endsr                                                  KJDO
     c*** begin add ***  05/06/11  ********************************************
    C*================================================================
    C* Load initial WDEFP records for the division.
    C*================================================================
     C     loadWDEFP     begsr

     c*** begin chg ***  08/19/11  *************************************************
     c     rsky1         chain     prs01
     c                   move      rsjrst        xxst
     c*
     C*****fnky          chain     pfi00
     c*****              dow       not(%eof)
     C     fnky          chain     pft01
     c                   dow       not(%eof)
     c*
     c     fiky          setgt     pfi00
     c     fiky1         readpe    pfi00                                  60
     c*
     c     *in60         ifeq      *off
     c*** end chg *****  08/19/11  *************************************************

      /free
       //Chain to file loads the fields to in1WDEFL1;
       exsr LoadKeyWDEFL1;
       chain(n) %kds(keyWDEFL1) pef01 in1WDEFL1;
       if not %found;
       //eval-corr outWDEFL1 = keyWDEFL1;
        exsr LoadOutWDEFL1;
        exsr MaintFldsInit;
        write pef01 outWDEFL1;
       endif;
      /end-free
     c*** begin add ***  08/19/11  *************************************************
     c                   endif
     c*** end add *****  08/19/11  *************************************************
     c
     c*** begin chg ***  08/19/11  *************************************************
     C*****fnky          reade     pfi00
     C     fnky          reade     pft01
     c*** end chg *****  08/19/11  *************************************************
     c                   enddo
     c
     C                   endsr                                                  loadWDEFP subr
     c*** end add *****  05/06/11  ********************************************
     C*
     C*===============================================================
     C* MaintFlds - Update the maintenance fields
     C*===============================================================
     C     MaintFlds     begsr
     C*
     C                   eval      EFUSER = q1User
     C                   eval      EFPGM  = q1Pgm
     C                   eval      EFTIME = TimeTo6(SystemTime)
     C                   eval      EFDT   = sysDt7
      /free
       //Load maintenace values;
       eval      outWDEFL1.EFPGM  = q1Pgm;
       eval      outWDEFL1.EFUSER = q1User;
       eval      outWDEFL1.EFDT   = sysDt7;
       eval      outWDEFL1.EFTIME = TimeTo6(SystemTime);
      /end-free
     C                   endsr
     C*
     C*===============================================================
     C* MaintFldsInit - Update the maintenance fields (initial)
     C*===============================================================
     C     MaintFldsInit begsr
      /free
       //Load maintenace values;
       exsr MaintFlds;
       //Load initial values;
       eval      outWDEFL1.EFIPGM = outWDEFL1.EFPGM;
       eval      outWDEFL1.EFIUSR = outWDEFL1.EFUSER;
       eval      outWDEFL1.EFIDT  = outWDEFL1.EFDT;
       eval      outWDEFL1.EFITM  = outWDEFL1.EFTIME;
      /end-free
     C                   eval      EFIUSR = EFUSER
     C                   eval      EFIPGM = EFPGM
     C                   eval      EFITM  = EFTIME
     C                   eval      EFIDT  = EFDT
     C*
     C                   endsr
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C     MVDBSC        begsr
     C*
     C                   IF        ELOGDF = 'Y' AND @@RRN <> *ZEROS
     C                   MOVEL     EFROBT        SCROBT
     C                   MOVEL     EFROFF        SCROFF
     C                   MOVEL     EFMOBT        SCMOBT
     C                   MOVEL     EFMOPN        SCMOPN
     C                   MOVEL     EFMOFF        SCMOFF
     C                   MOVEL     EFINBT        SCINBT
     C                   MOVEL     EFINPN        SCINPN
     C                   MOVEL     EFINFF        SCINFF
     C                   MOVEL     EFALBT        SCALBT
     C                   MOVEL     EFALFF        SCALFF
     C                   MOVEL     EFCOBS        SCCOBS
     C                   MOVEL     EFCOMA        SCCOMA
     C                   MOVEL     EFCOPN        SCCOPN
     C                   MOVEL     EFCOFF        SCCOFF
     C                   eval      scScBd = DtMDYY(EFSCBD)
      *
     C                   ELSE
     C
      * If Claim Fee Default is 'Y', then only system will default the
      * claim section of the screen...
      *
     C                   IF        *IN60 = '0'
      *
     C                   IF        FIDFED = 'Y'
      *
     C                   MOVEL     FIROBT        SCROBT
     C                   MOVEL     FIROFF        SCROFF
     C                   MOVEL     FIMOBT        SCMOBT
     C                   MOVEL     FIMOPN        SCMOPN
     C                   MOVEL     FIMOFF        SCMOFF
     C                   MOVEL     FIINBT        SCINBT
     C                   MOVEL     FIINPN        SCINPN
     C                   MOVEL     FIINFF        SCINFF
     C                   MOVEL     FIALBT        SCALBT
     C                   MOVEL     FIALFF        SCALFF
      *
     C                   ENDIF
      *
     C                   IF        FICDFE = 'Y'
      *
     C                   MOVEL     FICOBB        SCCOBS
     C                   MOVEL     FIMNCA        SCCOMA
     C                   MOVEL     FICOPN        SCCOPN
     C                   MOVEL     FICOFF        SCCOFF
      *
     C                   ENDIF                                                  FICDFE = 'Y'
      *
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   endsr                                                  MVDBSC
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C     MVSCDB        begsr
     C*
     C* SCREEN #1 FIELDS
     C                   MOVEL     ELCO#         EFCO#
     C                   MOVEL     ELFND         EFFND
     C                   MOVEL     ELEMP#        EFEMP#
     C                   MOVEL     ELDIV         EFDIV
     C                   MOVEL     ELFYR         EFFYR
     C                   eval      EFTYPE = FITYPE
     C                   eval      EFDESC = FIDESC
     C*
     C                   MOVEL     SCROBT        EFROBT
     C*                  MOVEL     SCROPN        EFROPN
     C                   MOVEL     SCROFF        EFROFF
     C                   MOVEL     SCMOBT        EFMOBT
     C                   MOVEL     SCMOPN        EFMOPN
     C                   MOVEL     SCMOFF        EFMOFF
     C                   MOVEL     SCINBT        EFINBT
     C                   MOVEL     SCINPN        EFINPN
     C                   MOVEL     SCINFF        EFINFF
     C                   MOVEL     SCALBT        EFALBT
     C*                  MOVEL     SCALPN        EFALPN
     C                   MOVEL     SCALFF        EFALFF
     C                   MOVEL     SCCOBS        EFCOBS
     C                   MOVEL     SCCOMA        EFCOMA
     C                   MOVEL     SCCOPN        EFCOPN
     C                   MOVEL     SCCOFF        EFCOFF
     C                   eval      EFSCBD = DtCYMD(scScBd)
     C*
     C                   endsr                                                  MVSCDB
     C*
     C*===============================================================
     C* OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
     C*===============================================================
     C     OTHERR        begsr
     C                   endsr                                                   OTHERR
     C*
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C     OTHFKS        begsr
     C*
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
     C*
     C                   endsr                                                  OTHFKS
     C*
     C*===============================================================
     C* PgmEnd - SUBROUTINE FOR ENDING THE PROGRAM
     C*===============================================================
     C     PgmEnd        begsr
     C                   MOVE      *ON           *INLR
     C                   endsr
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
     C*===============================================================
     C     PGMSTR        begsr
     C*
     C                   eval      sysDt7 = DateTo7(SystemDate)
     C*
     C                   exsr      LoadModeInd
     C*
     C                   Z-ADD     0             I1                3 0
     C*
     C*  GET EMPLOYER DETAIL RECORD
     C*
     C     @@RRN2        CHAIN     WDELP                              60
     C*
     C                   Z-ADD     ELFYR         SCFYR
     C*
     C*  GET EMPLOYER MASTER RECORD
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C                   Z-ADD     EMFND         SCFND
     C                   Z-ADD     EMEMP#        SCEMP#
     C                   Z-ADD     EMDIV         SCDIV
     c*** begin add ***  05/06/11  ********************************************
     c                   if        @@mode = 'A'
     c                   exsr      loadWDEFP
     c                   eval      @@mode = 'C'
     C                   exsr      LoadModeInd
     c                   endif
     c*** end add *****  05/06/11  ********************************************
     C*
     C*   Get Number of Employer Divisions to display in Screen Header
     C                   eval      sc#Div = DivCount(EMCO#:EMFND:EMEMP#)
     C*
     C*  GET AGENT RECORDS
     C*
     C                   Z-ADD     *ZEROS        ##FYR
     C                   CALL      AGTAPI        AGTPL
     C                   MOVEL     ##ANAM        SCANAM
     C                   MOVEL     ##PNAM        SCPNAM
     C*
     C                   eval      scEDN = EmpDivName(EMEMP#:EMDIV:emRRN:EMFND)
     C*
     C                   eval      scCxDt = GetCxDate(EMCO#:EMFND:EMEMP#:EMDIV)
     C                   IF        SCCXDT <> *ZEROS
     C                   EVAL      *IN70 = '1'
     C                   MOVE      *ON           *IN41
     C                   ELSE
     C                   EVAL      *IN70 = '0'
     C                   ENDIF
     C*
     C*  GET FUND MASTER INFORMATION
     C*
     C                   MOVEL     *BLANKS       SCNAME
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL     FNNAME        SCNAME
     C                   clear                   EXUPDA
     C     fnky          chain     pex00
     C*
     C                   dou       getOut
     C*
     C                   eval      ##Mode = @@Mode
     C                   eval      ##rrn  = @@rrn2
     C*
     C                   callp     W0063RB(*inkc:*inkl:##Mode:##rrn)
     C*
     C                   if        *inkc or *inkl
     C                   leave
     C                   endif
     C*
     C     ##rrn         chain     wtfip
     C                   if        not %found
     C                   leave
     C                   endif
     C*
     C                   exsr      LoadModeInd
     C*
     C                   eval      ##Mode = @@Mode
     C*
      /free
       //Chain to file loads the fields to in1WDEFL1;
       exsr LoadKeyWDEFL1;
       chain(n) %kds(keyWDEFL1) pef01 in1WDEFL1;
       if not %found;
       //eval-corr outWDEFL1 = keyWDEFL1;
        exsr LoadOutWDEFL1;
        exsr MaintFldsInit;
        write pef01 outWDEFL1;
        eval ##Mode = 'A';
        eval addMode = *on;
       endif;

       callp NextPgm(*inkc:*inkl);

       if addMode and (*inkc or *inkl);
        delete %kds(keyWDEFL1) pef01;
        iter;
       endif;

       //Chain to file loads the fields to in2WDEFL1;
       chain(n) %kds(keyWDEFL1) pef01 in2WDEFL1;

       //Compare file layouts from before and after extended program;
       if in1WDEFL1 <> in2WDEFL1;
        exsr UpdateAllDiv;
       endif;
      /end-free
     C*
     C                   enddo
     C*
     C                   exsr      PgmEnd
     C                   return
     C*
     C                   endsr                                                  PGMSTR
     C*
     C*===============================================================
     C* PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
     C*===============================================================
     C     PREOTH        begsr
     C                   endsr
     C*
     C*====================================================================
     C* UpdateAllDiv - Check to see if all divisions should be updated
     C*====================================================================
     C     UpdateAllDiv  begsr
     C*
     C     keyWDELL1     klist
     C                   kfld                    EFCO#
     C                   kfld                    EFFND
     C                   kfld                    EFFYR
     C                   kfld                    EFEMP#
     C*
     C                   if        not ProcessDivs
     C                   leavesr
     C                   endif
     C*
     C                   eval      holdDiv = ELDIV
     C                   eval      holdSt  = GetEmpSt(ELCO#:ELRTCD)
     C*
     C     keyWDELL1     setll     wdell1
     C                   dou       %eof
     C     keyWDELL1     reade     wdell1
     C                   if        not %eof and ELDIV <> holdDiv
      /free
       //Load key values;
       //Chain to file to determine if record exist for next division;
       exsr LoadKeyWDEFL1;
       chain %kds(keyWDEFL1) pef01;

       select;
       //Change mode;
       when updMode;
        if %found(WDEFL1);
         eval outWDEFL1 = in2WDEFL1;
         exsr MaintFlds;
         update pef01 outWDEFL1;
        endif;
       //Add mode;
       when addMode;
        if not %found(WDEFL1)
            and GetEmpSt(ELCO#:ELRTCD) = holdSt;
         eval outWDEFL1 = in2WDEFL1;
         exsr MaintFldsInit;
         write pef01 outWDEFL1;
        endif;
       endsl;
      /end-free
     C                   endif
     C                   enddo
     C*
     C                   endsr
     C*
     C*====================================================================
     C* UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
     C*====================================================================
     C     UPDATR        begsr
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   exsr      MaintFlds
     C                   UPDATE    DBRCDF
     C                   MOVEL     DBRRN         @@RRN
     C                   ELSE                                                   *IN99=*ON
     C                   EXSR      WRITER
     C                   ENDIF
     C*
     C                   MOVEL     *ON           OUTFLG
     C*
     C                   endsr                                                  UPDATR
     C*
     C*====================================================================
     C* WRITER - SUBROUTINE FOR WRITEING DATA BASE RECORDS
     C*====================================================================
     C     WRITER        begsr
     C*
     C                   clear                   dbRcdF
     C*
     C                   EXSR      MVSCDB
     C                   exsr      MaintFldsInit
     C                   WRITE     DBRCDF
     C*
     C                   feod      WDEFP
     C*
     C                   eval      @@rrn = dbRRN
     C*
     C                   MOVEL     *ON           OUTFLG
     C*
     C                   endsr                                                  WRITER





    P*====================================================================
    P* Get the current state the employer exists in
    P*====================================================================
     PGetEmpSt         b
     DGetEmpSt         pi                  like(RSJRST)
     D inCo#                               like(RSCO#)
     D inRtCd                              like(RSRTCD)

     C     keyWMRSL      klist
     C                   kfld                    inCo#
     C                   kfld                    inRtCd

     C                   clear                   RSJRST
     C     keyWMRSL      chain     WMRSL
     C                   return    RSJRST

     PGetEmpSt         e





    P*================================================================
    P* Process follow-on screens
    P*================================================================
     PNextPgm          b
     DNextPgm          pi             1n
     D F3                             1n
     D F12                            1n
     D*
     D getOut          s              1n   inz
     D inkc            s              1n
     D inkl            s              1n
     D xzTbId          s                   like(GTTBID) inz('FEES')

     c     keyWTGTL      klist
     c                   kfld                    ##co#
     c                   kfld                    xzTbId
     c                   kfld                    xxKey

     C                   eval      ##rrn  = efRRN
     C*
     C                   eval      xxKey = FITYPE
     C                   clear                   GTCF02
     C     keyWTGTL      chain     pgt00
     C                   call(e)   GTCF02        nextPgmPl
     C                   if        %error
     C                   eval      *inkl = *on
     C                   endif
     C*
     C                   eval      inkc  = *inkc
     C                   eval      inkl  = *inkl
     C                   eval      *inkc = *off
     C                   eval      *inkl = *off
     C                   eval      F3    = inkc
     C                   eval      F12   = inkl
     C*
     C                   return    getOut

    C*================================================================
    C* Finishing processes
    C*================================================================
     C     FinishNext    begsr

     C                   eval      getOut = *on

     C                   endsr

     PNextPgm          e





    P*====================================================================
    P* Determine if multiple divisions should be processed
    P*====================================================================
     PProcessDivs      b
     DProcessDivs      pi             1n

     C                   callp     PgmExistsInStk('W0090R':revised)
     C*
     C                   select
     C*
     C* Only process for revised bills
     C                   when      not revised
     C                   return    *off
     C*
    C* Do not process PEO type 'S' or 'G'
     C                   when      FNPEOF = 'S' or FNPEOF = 'G'
     C                   return    *off
     C*
     C* Only process if flag is set at group level
     C                   when      EXUPDA <> 'Y'
     C                   return    *off
     C*
     C* Only process if more than one division
     C                   when      sc#Div <= 1
     C                   return    *off
     C*
     C                   endsl
     C*
     C* Prompt user to determine if all divisions should be updated
     C                   dou       not *in90
     C                   exfmt     SCRDIV
     C                   select
     C                   when      scUpdate <> 'Y' and scUpdate <> 'N'
     C                   seton                                        71  90
     C                   endsl
     C                   enddo
     C*
     C                   return    scUpdate = 'Y'

     PProcessDivs      e
