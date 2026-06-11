     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*
     H* PROGRAM: F2305RDE - Procedure Code Information : Delaware
     H*
     H*****************************************************************
     F*  CREATED ON 03/03/2009 BY C:227 For Project DE004             *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  08/25/17  C:079  Support: 114/48786                          *
     H*                   Save screen fields to the database          *
     H*                                                               *
     H*  02/17/17  C:309  Project: DE014                              *
     H*                   Added new fields For Facility               *
     H*                                                               *
     H*  03/17/15  C:296  Project: DE012                              *
     H*                   Added new fields:                           *
     H*                   1) Outpatient Allowance                     *
     H*                   2) Outpatient POC                           *
     H*                   3) ASTC Allowance                           *
     H*                   4) ASTC POC                                 *
     H*                   5) Inpatient Allowance -Standard            *
     H*                                                               *
     H*  02/19/10  C:227  Project: DE006                              *
     H*                   Added new fields for Prof POC and Tech POC  *
     H*                   and changed Prof and Tech component fields  *
     H*
     H*****************************************************************
     FF2305FDE  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     F*** Beg Chg *** 08/25/17 ***********************************
     F***FMPRPDE   UF   E             DISK
     FFMPRPDE   UF A E             DISK
     F*** End Chg *** 08/25/17 ***********************************
     F                                     RENAME(PPR01:DBRCDF)
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Procedure Code-
     D                                      Information')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@ST              2
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C* PARM LIST FOR CALL TO STATE SPECIFIC PROGRAMS
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      @@RRN         ##RRN             9 0
     C                   PARM      @@ST          ##ST              2
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*===============================================================
     C* DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
     C*===============================================================
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C     CK1END        ENDSR
     C*
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C*
     C                   ENDSR
     C*================================================================
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL(P)  PRCPT4        SCCPT4
     C*
     C                   Z-ADD     PREFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   Z-ADD     PRMRA         SCMRA
     C                   Z-ADD     PRPROF        SCPROF
     C*** BEG CHG *** 02/19/10 ****************************************
     C***********          Z-ADDPRPROF    SCPRF1
     C                   Z-ADD     PR92NI        SCPRF1
     C*** END CHG *** 02/19/10 ****************************************
     C*
     C                   Z-ADD     PR92N2        SC92N2
     C*** BEG CHG *** 02/19/10 ****************************************
     C***********          Z-ADDPR92N2    SC92NX
     C                   Z-ADD     PR92NJ        SC92NX
     C*** END CHG *** 02/19/10 ****************************************
     C                   Z-ADD     PR92N3        SC92N3
     C                   Z-ADD     PR92N4        SC92N4
     C                   Z-ADD     PR92N5        SC92N5
     C*
     C                   Z-ADD     PR92NA        SC92NA
     C                   Z-ADD     PR92NB        SC92NB
     C                   Z-ADD     PR92NC        SC92NC
     C                   Z-ADD     PR92ND        SC92ND
     C                   Z-ADD     PR92NE        SC92NE
     C                   Z-ADD     PR92NF        SC92NF
     C                   Z-ADD     PR92NG        SC92NG
     C                   Z-ADD     PR92NH        SC92NH
     C*** BEG ADD *** 02/19/10 ****************************************
     C                   Z-ADD     PR92NK        SC92NK
     C                   Z-ADD     PR92NM        SC92NM
     C                   Z-ADD     PR92NL        SC92NL
     C                   Z-ADD     PR92NN        SC92NN
     C*** END ADD *** 02/19/10 ****************************************
     C*
     C                   MOVEL(P)  PRSTAT        SCSTAT
     C                   MOVEL(P)  PRDSC1        SCDSC1
     C                   MOVEL(P)  PRDSC2        SCDSC2
     C*
     C*** Beg Add *** 03/17/15 ****************************************
      /Free
       SC92N6 = PR92N6;
       SC92N7 = PR92N7;
       SC92N8 = PR92N8;
       SC92N9 = PR92N9;
       SC92Nq = PR92Nq;
       SC92Nr = PR92Nr;
       SC92Ns = PR92Ns;
       SC92Nt = PR92Nt;
       SC92Nu = PR92Nu;
       SC92Nv = PR92Nv;

       //*** Beg Add *** 02/17/17 *************************************
       sc92nw = PR92nw ;
       sc92ny = PR92ny ;
       sc92nz = PR92nz ;
       sc92o1 = PR92o1 ;
       sc92o2 = PR92o2 ;
       sc92o3 = PR92o3 ;
       sc92o4 = PR92o4 ;
       sc92o5 = PR92o5 ;
       sc92o6 = PR92o6 ;
       sc92o7 = PR92o7 ;
       sc92o8 = PR92o8 ;
       sc92o9 = PR92o9 ;
       //*** End Add *** 02/17/17 *************************************

      /End-Free
     C*** End Add *** 03/17/15 ****************************************
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C*** Beg Add *** 03/17/15 ****************************************
      /Free
       PRMRA  = SCMRA;
       PRPROF = SCPROF;
       PR92NI = SCPRF1;
       PR92N2 = SC92N2;
       PR92NJ = SC92NX;
       PR92N3 = SC92N3;
       PR92N4 = SC92N4;
       PR92N5 = SC92N5;
       PR92NA = SC92NA;
       PR92NB = SC92NB;
       PR92NC = SC92NC;
       PR92ND = SC92ND;
       PR92NE = SC92NE;
       PR92NF = SC92NF;
       PR92NG = SC92NG;
       PR92NH = SC92NH;
       PR92NK = SC92NK;
       PR92NM = SC92NM;
       PR92NL = SC92NL;
       PR92NN = SC92NN;
       PRDSC1 = SCDSC1;
       PRDSC2 = SCDSC2;
       PR92N6 = SC92N6;
       PR92N7 = SC92N7;
       PR92N8 = SC92N8;
       PR92N9 = SC92N9;
       PR92Nq = SC92Nq;
       PR92Nr = SC92Nr;
       PR92Ns = SC92Ns;
       PR92Nt = SC92Nt;
       PR92Nu = SC92Nu;
       PR92Nv = SC92Nv;

       //*** Beg Add *** 02/17/17 *************************************
       PR92nw =  sc92nw ;
       PR92ny =  sc92ny ;
       PR92nz =  sc92nz ;
       PR92o1 =  sc92o1 ;
       PR92o2 =  sc92o2 ;
       PR92o3 =  sc92o3 ;
       PR92o4 =  sc92o4 ;
       PR92o5 =  sc92o5 ;
       PR92o6 =  sc92o6 ;
       PR92o7 =  sc92o7 ;
       PR92o8 =  sc92o8 ;
       PR92o9 =  sc92o9 ;
       //*** End Add *** 02/17/17 *************************************

      /End-Free
     C*** End Add *** 03/17/15 ****************************************
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   RETURN
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
       //*** Beg Add *** 08/25/17 ************************************
      /Free
           Chain @@rrn fmprpde;
B01        If not %eof(fmprpde);
B01           exSr mvscdb;
B01           PRuser = Q1user;
B01           PRpgm  = Q1pgm;
              PRdt   = %dec(%char(%date():*cymd0):7:0);
              PRtime = %dec(%char(%time():*iso0):6:0);
B01           Update dbrcdf;
B01        Endif;
      /End-free
       //*** End Add *** 08/25/17 ************************************
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   ENDSR
