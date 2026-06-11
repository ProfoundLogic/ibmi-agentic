      * (C) Copyright Progressive Data Solutions, Inc., 2001 - 2004         *
      *                                                                =====*
     h/title W8700R4: Assign carrier # on quote transfer
     H option(*nodebugio: *srcstmt)                                                                *
     H dftactgrp(*no) actgrp(*caller)
      *=====================================================================*
      * Program description:                                                *
      *   This program assigns a carrier policy number for an employer      *
      *   during quote transfer based on the criteria assigned for the      *
      *   group.                                                            *
      *                                                                     *
      * Called from: W8700R                                                 *
      * Calls to   : None                                                   *
      *                                                                     *
      * Programmer : C: 90                                                  *
      * Created    : 10/31/01                                               *
      * Project    : WC1054                                                 *
      *=====================================================================*
      * REVISIONS:                                                          *
      *                                                                     *
      * 07/29/05 C:09  SUPPORT WA135 48330                                  *
      *                If Client #=135 and Policy Eff Date < 6/01/2005,     *
      *                then do NOT auto-assign Carrier Policy #.  Instead   *
      *                assign the carrier policy # from division 0.         *
      *                If division=0, then auto-assign as normal.           *
      *                                                                     *
      * 06/25/04 C:61  SUPPORT WA099 43023                                  *
      *                Change state field to 2 characters.                  *
      *                                                                     *
      * 02/18/02 C:90  PROJECT: Seq#31306                                   *
      *                Load maintenance fields in WMRPP.                    *
      *                                                                     *
      * XX/XX/XX C:XXX PROJECT: XXXXX                                       *
      *                XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX       *
      *                                                                     *
      * ------------------------------------------------------------------- *
      *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS                  *
      * ------------------------------------------------------------------- *
      *=====================================================================*

     Hdatedit(*ymd) datfmt(*ymd)

     FWMRPL     uf a e           k disk                                         Output Routing
     FWMGCL     if   e           k disk                                         Group Master
     FWMRSL     if   e           k disk                                         User Parameters
     FWDELL     if   e           k disk                                         Employer End logical
      ******** Beg Add ****** 07/29/05 *************************************
     Fwmrpl4    if   e           k disk    rename(prp01:prp00) prefix(d0_)
      ******** End Add ****** 07/29/05 *************************************
     FQSYSPRT   O    F  126        printer oflind(*inob)

      *=============================================================================================
      *  Data Structure definitions
      *=============================================================================================
      ** Working storage
     d  CLIENT         ds           256    inz
     dWorkStor         ds                  inz                                   Work/Storage Area
     d  xxDATE                        8s 0                                         Date
     d    xxYEAR                      4s 0 overlay(xxDATE:1)                         Year
     d    xxMNTH                      2s 0 overlay(xxDATE:5)                         Month
     d    xxDAY                       2s 0 overlay(xxDATE:7)                         day

      *=============================================================================================
      *  Field definitions
      *=============================================================================================
      **** begin add **** 02/18/02 *****************************************
     d                 ds
     d dsdttm                        14  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        8  0 overlay(dsdttm:7)
    
     d srdti           s              8  0
     d srdto           s             10
     d srerr           s              1
     d srffmt          s              8
     d srtfmt          s              8
      **** end   add **** 02/18/02 *****************************************
    
     d  cpol           s              1a   dim(18)
     d  newpol         s              1a   dim(18)
     d  ##yr2          s              1a   dim(2)
     d  ##yr3          s              1a   dim(3)
      ******** beg chg ****** 06/25/04 *************************************
     d**##st******     s              1a   dim(3)
     d  ##st           s              1a   dim(2)
      ******** end chg ****** 06/25/04 *************************************
     d  numl           s              2  0
     d  ##fnd          s              1a   dim(3)
     d  ##num          s              1a   dim(9)

     d  ##fyr          s              3  0
     d  @@fyr          s              3a   inz('000')
     d  @@nacn         s              9a
     d  @@empn         s              9a   varying
     d  @@fnd          s              3a
     d  ##empn         s              9a

     d  x              s              2  0 inz(1)
     d  yr             s              2  0
     d  st             s              2  0
     d  fnd            s              2  0
     d  emp            s              2  0
     d  empst          s              2  0
     d  empe           s              2  0
     d  empend         s              2  0
     d  grp            s              2  0
      ******** Beg Add ****** 07/29/05 *************************************
     d  xxdiv0         s              5  0 inz(0)
     d  xxpfdt         s                   like(ELPFDT)
      ******** End Add ****** 07/29/05 *************************************

      **** begin add **** 02/18/02 *****************************************
     d  psds         esds                  extname(szq1p)
      **** end   add **** 02/18/02 *****************************************

      ** Temp variables
     d**xxCompany      s                   like(ENCO#)
      *=============================================================================================
      *  PLIST definitions
      *=============================================================================================
      ** Entry parameters
     c     *entry        plist
     c                   parm                    xxco#             3 0           Company
     c                   parm                    xxfnd             3 0           Group
     c                   parm                    xxemp#            9 0           Employer
     c                   parm                    xxdiv             5 0           Division
     c                   parm                    xxfyr             3 0           Year
    c*
     c     dtcymd        plist
     c                   parm                    srdti
     c                   parm      *blanks       srffmt
     c                   parm      '*CYMD'       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
    c*
      *=============================================================================================
      *  KLIST definitions
      *=============================================================================================
      ** Employer Detail File
     c     elky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    xxemp#
     c                   kfld                    xxdiv
     c                   kfld                    xxfyr

     c     elky1         klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    xxemp#
     c                   kfld                    xxdiv

      ** Carrier State/Reporting File
     c     rpky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    xxemp#
     c                   kfld                    xxdiv
     c                   kfld                    xxfyr

      ******** Beg Add ****** 07/29/05 *************************************
     c     rpky2         klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    xxfyr
     c                   kfld                    xxemp#
     c                   kfld                    xxdiv0
      ******** End Add ****** 07/29/05 *************************************

      ** Group Carrier Information
     c     gcky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd

      ** Rate Set Code File
     c     rsky          klist
     c                   kfld                    elco#
     c                   kfld                    elrtcd

      *=============================================================================================
      *  Main
      *=============================================================================================
      ** Assign carrier policy number

     c                   EVAL      x = 1

     c     gcky          CHAIN     pgc01                              60

     c     elky          CHAIN     pel01                              60
     c                   MOVE      elpfdt        xxpfdt
     c     rsky          CHAIN     prs01                              60

     c                   MOVEA     rsjrst        ##st
     c                   MOVE      gcnacn        @@nacn
     c                   MOVE      elemp#        ##empn
     c                   MOVE      elfnd         @@fnd

     c                   MOVEA     gccfmt        cpol
     c     '@@@'         SCAN      gccfmt        yr                       51
     c  n51'@@'          SCAN      gccfmt        yr                       52
     c  n52
     cann51'%%%'         SCAN      gccfmt        yr                       53

     c     '&&'          SCAN      gccfmt        st                       54

     c     '#'           SCAN      gccfmt        emp                      55
     c     '$'           SCAN      gccfmt        emp                      56

     c                   EVAL      empe = emp
     c   55'#'           LOOKUP    cpol(empe)                             60
     c   56'$'           LOOKUP    cpol(empe)                             60
     c     *in60         DOWEQ     *on
     c                   ADD       1             x
     c                   EVAL      empe = 1 + empe
     c   55'#'           LOOKUP    cpol(empe)                             60
     c   56'$'           LOOKUP    cpol(empe)                             60
     c   60              EVAL      numl = x
     c   60              EVAL      empend = empe
     c                   ENDDO

     c                   EVAL      %len(@@empn) = numl

     c     '+'           SCAN      gccfmt        grp                      57

     c   55              MOVEA     @@nacn        ##num
     c                   EVAL      empst = 10 - numl
     c   56numl          SUBST     ##empn:empst  @@empn
     c   57              MOVEA     @@fnd         ##fnd

     c                   EVAL      ##fyr = 0
     c     elky1         SETLL     pel01
     c     elky1         READE     pel01                                  61
     c     *in61         DOWEQ     *off
     c                   ADD       1             ##fyr
     c     elky1         READE     pel01                                  61
     c                   ENDDO

     c                   MOVE      ##fyr         @@fyr
     c   53              MOVE      elfyr         @@fyr
     c   51              MOVEA     @@fyr         ##yr3
     c   52              MOVEA     @@fyr         ##yr2
     c   53              MOVEA     @@fyr         ##yr3

     c                   MOVEA     gccfmt        cpol
     c   51              MOVEA     ##yr3         cpol(yr)
     c   52              MOVEA     ##yr2         cpol(yr)
     c   53              MOVEA     ##yr3         cpol(yr)
     c   54              MOVEA     ##st          cpol(st)
     c   55              MOVEA     ##num         cpol(emp)
     c   56              MOVEA     @@empn        cpol(emp)
     c   57              MOVEA     ##fnd         cpol(grp)

     c                   EXSR      createcpol

     c                   RETURN

     c*=====================================================================
     c* createcpol - subroutine to create carrier policy number
     c*=====================================================================

     c     createcpol    BEGSR

     c                   EVAL      rpco# = xxco#
     c                   EVAL      rpfnd = xxfnd
     c                   EVAL      rpemp# = xxemp#
     c                   EVAL      rpdiv = xxdiv
     c                   EVAL      rpfyr = xxfyr

     c                   EVAL      rpcarr = gcdcar
     c                   MOVEA     cpol          rpcap#

      **** begin add **** 02/18/02 *****************************************
     c                   EVAL      rpiusr = q1user
     c                   EVAL      rpuser = q1user
     c                   EVAL      rpipgm = q1pgm
     c                   EVAL      rppgm = q1pgm

     c                   time                    dsdttm
     c                   Z-ADD     dsdate        srdti
     c                   CALL      'SRCVTDAT'    dtcymd
     c                   MOVEL     srdto         rpidt
     c                   MOVEL     srdto         rpdt

     c                   EVAL      rpitm = dstime
     c                   EVAL      rptime = dstime
      **** end   add **** 02/18/02 *****************************************
      ******** Beg Add ****** 07/29/05 *************************************
     C                   call      'RTVCLTID'
     C                   parm                    client#           3
     C                   If        client# = '135' and xxpfdt < 1050601
     C                               and xxdiv <> 0
     C     rpky2         Chain     prp00                              60
     C                   If        *IN60 = '0'
     C                   EVAL      rpcap# = d0_rpcap#
     C                   Endif
     C                   Endif
      ******** End Add ****** 07/29/05 *************************************

     c                   WRITE     prp01

     c                   ENDSR
