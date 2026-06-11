     H*********************************************************************
     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H*********************************************************************
     H/TITLE W7429R - PRINT AGENT ACCOUNT CURRENT STATEMENTS
     H*********************************************************************
     H* PROGRAM DESCRIPTION:                                              *
     H*   THIS PROGRAM PRODUCES A STATEMENT BASE UPON THE AGENCY BILLING  *
     H*   DETAIL (WDABP) AND CAN BE RE-RUN AT ANY TIME.  THE STATEMENT    *
     H*   WILL LIST EACH EMPLOYER AND SHOW A LINE ITEM DETAIL FOR EACH    *
     H*   TRANSACTION (WDABP), INCLUDING THE ITEMS WHICH DON'T RECEIVE    *
     H*   COMMISSION.                                                     *
     H*                                                                   *
     H*  CALLED FROM:                                                     *
     H*    W7429C                                                         *
     H*  CALLS TO:                                                        *
     H*    NONE                                                           *
     H*                                                                   *
     H*  CREATED: 05/03/99     C:33    PROJECT: 96WAD                     *
     H*********************************************************************
     H*  REVISIONS:                                                       *
     H*                                                                   *
     H* 08/26/14 C:307 Project: SY354                                     *
     H*                -Implement Routing Page Changes.                   *
     H*                                                                   *
     H* 08/18/14  C:33   Project: SY354                               *
     H*                  Remove extraneous 'routing' info.            *
     H*                                                                   *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  08/23/06  C:09   Seq#: WA132/54328                               *
     H*                   Apply updates to ensure True Voids ('T' types)  *
     H*                   are listed properly on Account Current Stmt     *
     H*                                                                   *
     H*  07/06/06  C:33   Seq#: WA132/52810                               *
     H*                   Process refunds like payments.                  *
     H*                                                                   *
     H*  11/05/04  C:132  SUPPORT: WA132  44359                           *
     H*                   Program was including employer information from *
     H*                   Test Groups. Add code to check Test group flag. *
     H*                                                                   *
     H*  07/02/01  C:132  SUPPORT: WA096/27914                            *
     H*                   Program did not subtract out the Fee Payments   *
     H*                   from Net Amount Due                             *
     H*                                                                   *
     H*  04/04/01  C:132  SUPPORT: WA096/26956                            *
     H*                   Save Agent Heading Varibles so when Agent Chgs  *
     H*                   will print correct information on Total Page    *
     H*                      For the Agent, then get new Agent Information*
     H*                                                                   *
     H*  01/07/00  C:118  SUPPORT: WA107/21247                            *
     H*                   Shift the following columns and headings to the *
     H*                   left 3 spaces so report prints correctly:       *
     H*                      Amount, rate, comm amt, net amt, date, page  *
     H*                      statement date and due date and totals.      *
     H*                                                                   *
     H*  12/10/99  C:33  Y2K#: 32                                         *
     H*                  BE SURE TO PROCESS ALL RECORDS.                  *
     H*                                                                   *
     H*  11/19/99  C:33  SEQ#: WA107/20545                                *
     H*                  KEEP DETAIL LINES FROM OVERLAPPING               *
     H*********************************************************************
     FSMORL     IF   E           K disk
     FSMCOL     IF   E           K disk
     FWMUPL     IF   E           K disk
     FWDABL1    IF   E           K disk
     FWSASL1    IF   E           K disk
     FWMAAL5    IF   E           K disk
     FWMAGL     IF   E           K disk
     FWMAPL     IF   E           K disk
     FWDELL2    IF   E           K disk
     FWMEML     IF   E           K disk
     F*
     F***** BEG ADD ***** 11/05/04 ****************************************
     FWMFNL     IF   E           K disk
     F***** END ADD ***** 11/05/04 ****************************************
     F*
     F***  BEGIN ADD  ***  04/04/01  **************************************
     F*  Chg Indicator for Overflow to OB, since used thruout Pgm
     F*SYSPRT   O    F  126        printer oflind(*inoa)
     FQSYSPRT   O    F  126        printer oflind(*inob)
     F***  END   ADD  ***  04/04/01  **************************************
     D                 DS
     D DSDTTM                        14  0
     D  DSTIME                        6  0 overlay(DSDTTM)
     D  DSDATE                        8  0 overlay(DSDTTM:7)
     D                 DS
     D X7STDT                         7  0 inz
     D  X7STDT_MM                     2  0 overlay(X7STDT:4)
     D  X7STDT_DD                     2  0 overlay(X7STDT:6)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     *---------------------------------------------------------------------
     * Stand Alone Fields - TOP
     *---------------------------------------------------------------------
     D x0title         c                   'Agent Account Current Statement'    t')
     D @@co#           s              3
     D @@stdt          s              6
     D @@agt#          s              5
     D @@que           s             10
     D xxco#           s                   like(ABCO#)
     D x6stdt          s              6  0
     D xxagt#          s                   like(ABAGT#)
     D xxque           s                   like(@@que)
     D x5stdt          s                   like(ABSTDT)
     D xxprt           s                   like(ORFIL#)
     D svagt#          s                   like(ABAGT#)
     D svprod          s                   like(ABPROD)
     D svfnd           s                   like(ABFND)
     D svemp#          s                   like(ABEMP#)
     D svdiv           s                   like(ABDIV)
     D x7date          s              7  0
     D x8due_usa       s               D   datfmt(*USA)
     D x8pfdt_usa      s               D   datfmt(*USA)
     D***  BEGIN ADD  ***  04/04/01  **************************************
     D xxDUE_DTE       s               D   datfmt(*USA)
     D xxNAME          s                   inz like(AGMAD1)
     D xxMAD1          s                   inz like(AGMAD1)
     D xxCSZ           s                   inz like(AGMAD1)
     D***  END   ADD  ***  04/04/01  **************************************
     D AGCSZ           s                   like(AGMAD1)
     D APNAME          s             47
     D xxzip           s             10
     D xxzip1          s              5
     D xxzip2          s              4
     D prod_amt        s                   inz like(ABAMT)
     D prod_comm       s                   inz like(ABCOMM)
     D prod_net        s                   inz like(ABNET)
     D agt_amt         s                   inz like(ABAMT)
     D agt_comm        s                   inz like(ABCOMM)
     D agt_net         s                   inz like(ABNET)
     D agt_prev        s                   inz like(ABNET)
     D agt_due         s                   inz like(ABNET)
     D total_amt       s                   inz like(ABAMT)
     D total_comm      s                   inz like(ABCOMM)
     D total_net       s                   inz like(ABNET)
     D total_prev      s                   inz like(ABNET)
     D total_due       s                   inz like(ABNET)
     D break_lvl       s              1
     D last_rcd        s              1
     D ##data          s             80
     D ##length        s              3  0
     D n               s              3  0
     D***  Begin Add  ***  01/28/10  **************************************
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************
     *---------------------------------------------------------------------
     * Stand Alone Fields - BOTTOM
     *---------------------------------------------------------------------
     C*
     C/COPY SCOPYDTPL
     C*
     C     orky          klist
     C                   kfld                    Q1PGM
     C                   kfld                    xxprt
     C*
     C     abky          klist
     C                   kfld                    xxco#
     C                   kfld                    x5stdt
     C*
     C     abky1         klist
     C                   kfld                    xxco#
     C                   kfld                    x5stdt
     C                   kfld                    xxagt#
     C*
     C     asky          klist
     C                   kfld                    xxco#
     C                   kfld                    ABAGT#
     C                   kfld                    x5stdt
     C*
     C     asky1         klist
     C                   kfld                    xxco#
     C                   kfld                    svagt#
     C*
     C     aaky          klist
     C                   kfld                    xxco#
     C                   kfld                    ABAGT#
     C*
     C     agky          klist
     C                   kfld                    xxco#
     C                   kfld                    AAFEIN
     C                   kfld                    AAMOD
     C*
     C     apky          klist
     C                   kfld                    xxco#
     C                   kfld                    AAFEIN
     C                   kfld                    AAMOD
     C                   kfld                    ABPROD
     C*
     C     elky          klist
     C                   kfld                    xxco#
     C                   kfld                    ABFND
     C                   kfld                    ABEMP#
     C                   kfld                    ABDIV
     C                   kfld                    x7date
     C*
     C     elky2         klist
     C                   kfld                    xxco#
     C                   kfld                    ABFND
     C                   kfld                    ABEMP#
     C                   kfld                    ABDIV
     C*
     C     emky          klist
     C                   kfld                    xxco#
     C                   kfld                    ABFND
     C                   kfld                    ABEMP#
     C                   kfld                    ABDIV
      *
      ***** BEG ADD ***** 11/05/04 ****************************************
     C*  Group Master file key
     C     fnky          klist
     C                   kfld                    xxco#
     C                   kfld                    SVFND
      ***** END ADD ***** 11/05/04 ****************************************
     C*____________________________________________________________________
     C*
     C     *ENTRY        plist
     C                   parm                    @@co#
     C                   parm                    @@stdt
     C                   parm                    @@agt#
     C                   parm                    @@que
     C*____________________________________________________________________
     C*
     C*  READ AGENCY BILLING DETAIL FILE
     C*
     C   20abky          chain     PAB01                              61
     C  N20abky1         chain     PAB01                              61
     C*
     C                   if        *in61 = *on
     C*
     C                   except    TTL_NOREC
     C*
     C                   else
     C*
     C                   eval      svagt# = ABAGT#
     C                   eval      svprod = ABPROD
     C                   eval      svemp# = ABEMP#
     C                   eval      svdiv  = ABDIV
     C                   eval      svfnd  = ABFND
      *
      ***** BEG ADD ***** 11/05/04 ****************************************
     C     FNKY          chain     PFN01                              61
      ***** END ADD ***** 11/05/04 ****************************************
     C*
     C                   exsr      AGT_NAME
     C***  BEGIN ADD  ***  04/04/01  **************************************
     C                   move      AGNAME        xxNAME
     C                   move      AGMAD1        xxMAD1
     C                   move      AGCSZ         xxCSZ
     C                   move      x8due_usa     xxdue_dte
     C***  END   ADD  ***  04/04/01  **************************************
     C                   exsr      PROD_NAME
     C                   exsr      EMP_NAME
     C                   exsr      PRT_HEAD
     C*
     C***  BEGIN ADD  ***  12/10/99  **************************************
     C                   eval      *in61 = *off
     C***  END   ADD  ***  12/10/99  **************************************
     C*
     C                   dow       *in61 = *OFF
      *
      ***** BEG ADD ***** 11/05/04 ****************************************
     C                   IF        FNTEST <> 'Y'
     C                   exsr      PROCESS
     C                   EndIF
      ***** END ADD ***** 11/05/04 ****************************************
      ***** BEG DEL ***** 11/05/04 ****************************************
     C***************    exsr      PROCESS
      ***** END DEL ***** 11/05/04 ****************************************
     C*
     C   20abky          reade     PAB01                                  61
     C  N20abky1         reade     PAB01                                  61
     C*
     C                   enddo
     C*
     C                   eval      last_rcd = *on
     C                   eval      break_lvl = 'A'
     C                   exsr      AGT_BRK
     C                   except    HEAD
     C                   except    TOTAL
     C*
     C                   endif                                                  *in60=*on
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orky POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   eval      *inlr = *on
     C*____________________________________________________________________
     C*
     C*====================================================================
     C* *inzsr - Initialization subroutine
     C*====================================================================
     C*
     C     *inzsr        BEGSR
     C*
     C                   move      @@co#         xxco#
     C                   move      @@stdt        x6stdt
     C*
     C                   eval      SRDTI = x6stdt
     C                   call      'SRCVTMY'     DTCYMD                 99
     C                   movel     SRDTO         x5stdt
     C*
     C                   if        Q1PRMS >= 3
     C                   move      @@agt#        xxagt#
     C                   else
     C                   clear                   xxagt#
     C                   endif                                                  Q1PRMS>=3
     C*
     C                   eval      *in20 = xxagt# = *zeros
     C*
     C                   if        Q1PRMS >= 4
     C                   eval      xxque = @@que
     C                   else
     C                   clear                   xxque
     C                   endif                                                  Q1PRMS>=4
     C*
     C                   eval      *in09 = xxque <> *BLANK
     C                   eval      *in08 = xxque = *BLANK
     C*
     C                   TIME                    DSDTTM
     C*
     C                   z-add     DSDATE        SRDTI
     C                   call      'SRCVTDAT'    DTCYMD
     C                   movel     SRDTO         x7date
     C*
     C*  GET USER, COMPANY, AND GROUP INFORMATION
     C*
     C     Q1USER        chain     PUP01                              60             "
     C     xxco#         chain     PCO01                              60
     C*
     C                   movel(P)  CONAME        ##data
     C*
     c*** Beg Add ***** 01/28/10 **************************************
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
     c*** End add ***** 01/28/10 **************************************
     C*
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##length
     C*
     C                   movel     ##data        CONAME
     C*
     C                   eval      xxprt = '0'
     C*
     C*  GET OUTPUT ROUTING RECORD
     C*
     C     orky          chain     POR01                              60
     C*
     C*  Left Justify Title in Field
     C*
     C                   if        ORTITL <> *BLANKS                            FIELD
     C                   move(P)   ORTITL        ##data                         FIELD, LENGTH
     C     ' '           CHECK     ##data        n
     C                   if        n <= 60                                      LENGTH
     C                   subst(P)  ##data:N      ORTITL                         FIELD
     C                   endif                                                  N<=60
     C                   endif                                                  ORTITL<>*BLANKS
     C*
     C                   if        *in60 = *off
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         except    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   else
     C                   eval      ORTITL = x0title
     C                   endif
     C*
     C*  CENTER TEXT IN FIELD
     C*
     C                   movel     ORTITL        ##data
     C*
     C                   call      'SRCENTER'                           99
     C                   parm                    ##data
     C                   parm      60            ##length
     C*
     C                   movel     ##data        ORTITL
     C*
     C*******            except    HEAD
     C*
     C                   eval      last_rcd = *off
     C*
     C                   ENDSR
     C*
     C*====================================================================
     C* AGT_BRK - AGENT BREAK
     C*====================================================================
     C*
     C     AGT_BRK       BEGSR
     C*
     C                   exsr      AGT_NAME
     C*
     C                   exsr      PROD_BRK
     C*
     C                   exsr      PREV_BAL
     C*
     C                   if        *inob = *on
     C                   except    HEAD
     C                   except    HEAD_AGT
     C                   eval      *inob = *off
     C                   endif                                                  *inob=*on
     C*
     C                   except    AGT_TOTAL
     C*
     C                   eval      svagt# = ABAGT#
     C***  BEGIN ADD  ***  04/04/01  **************************************
     C* Save Agent Info for Print Headings on Agent Breaks
     C                   eval      xxNAME = *blanks
     C                   eval      xxMAD1 = *blanks
     C                   eval      xxCSZ  = *blanks
     C                   move      AGNAME        xxNAME
     C                   move      AGMAD1        xxMAD1
     C                   move      AGCSZ         xxCSZ
     C                   move      x8due_usa     xxdue_dte
     C***  END   ADD  ***  04/04/01  **************************************
     C                   eval      PAGE = 0
     C                   eval      *inob = *on
     C*
     C                   if        last_rcd = *off
     C                   exsr      PRT_HEAD
     C                   endif                                                  last_rcd=*off
     C*
     C                   eval      total_amt  = total_amt  + agt_amt
     C                   eval      total_comm = total_comm + agt_comm
     C                   eval      total_net  = total_net  + agt_net
     C                   eval      total_prev = total_prev + agt_prev
     C                   eval      total_due  = total_due  + agt_due
     C*
     C                   clear                   agt_amt
     C                   clear                   agt_comm
     C                   clear                   agt_net
     C                   clear                   agt_prev
     C                   clear                   agt_due
     C*
     C                   ENDSR
     C*
     C*====================================================================
     C* AGT_NAME - GET THE AGENT NAME FROM MASTER FILE
     C*====================================================================
     C*
     C     AGT_NAME      BEGSR
     C*
     C     aaky          chain     PAA01                              61
     C*
     C                   if        *in61 = *on
     C                   clear                   AAFEIN
     C                   clear                   AAMOD
     C                   endif
     C*
     C     agky          chain     PAG01                              61
     C*
     C                   if        *in61 = *on
     C                   clear                   AGNAME
     C                   clear                   AGMAD1
     C                   clear                   AGMST
     C                   clear                   AGMCTY
     C                   clear                   AGMZIP
     C                   clear                   AGPAD1
     C                   clear                   AGPST
     C                   clear                   AGPCTY
     C                   clear                   AGPZIP
     C                   clear                   AGACDU
     C                   endif
     C*
     C                   if        AGMAD1 = *blanks
     C                   eval      AGMAD1 = AGPAD1
     C                   eval      AGMST  = AGPST
     C                   eval      AGMCTY = AGPCTY
     C                   eval      AGMZIP = AGPZIP
     C                   endif                                                  AGMAD1 = *blanks
     C*
     C                   movel     AGMZIP        xxzip1
     C                   move      AGMZIP        xxzip2
     C*
     C                   if        xxzip2 <> *ZEROS
     C                   eval      xxzip = xxzip1 + '-' + xxzip2
     C                   else
     C                   eval      xxzip = xxzip1
     C                   endif
     C*
     C     AGMCTY        cat(P)    ',':0         AGCSZ
     C                   cat       AGMST:1       AGCSZ
     C                   cat       xxzip:2       AGCSZ
     C*
     C     asky          chain     PAS01                              60
     C*
     C                   if        *in60 = *off
     C     *cymd         move      ASDUE         x8due_usa
     C                   else
     C                   exsr      CALC_DUE
     C                   endif                                                  *in60=*off
     C*
     C                   ENDSR
     C*
     C*====================================================================
     C* CALC_DUE - Calculate the due date
     C*====================================================================
     C*
     C     CALC_DUE      BEGSR
     C*
     C                   movel     x5stdt        X7STDT
     C*
     C                   select
     C                   when      X7STDT_MM = 2
     C                   eval      X7STDT_DD = 28
     C                   when      X7STDT_MM = 4
     C                              or X7STDT_MM = 6
     C                              or X7STDT_MM = 9
     C                              or X7STDT_MM = 11
     C                   eval      X7STDT_DD = 28
     C                   other
     C                   eval      X7STDT_DD = 31
     C                   endsl
     C*
     C     *cymd         move      X7STDT        x8due_usa
     C*
     C                   if        *in60 = *off
     C                   adddur    AGACDU:*D     x8due_usa
     C                   endif                                                  *in60=*off
     C*
     C                   ENDSR                                                  CALC_DUE
     C*
     C*====================================================================
     C* EMP_BRK - EMPLOYER BREAK
     C*====================================================================
     C*
     C     EMP_BRK       BEGSR
     C*
     C                   exsr      EMP_NAME
     C*
     C                   if        break_lvl = 'E'
     C                   exsr      PRT_PROD
     C                   except    HEAD_EMP
     C                   endif                                                  break_lvl=E
     C*
     C                   eval      svemp# = ABEMP#
     C                   eval      svdiv  = ABDIV
     C                   eval      svfnd  = ABFND
      *
      ***** BEG ADD ***** 11/05/04 ****************************************
     C     FNKY          chain     PFN01                              61
      ***** END ADD ***** 11/05/04 ****************************************
     C*
     C                   ENDSR                                                  EMP_BRK
     C*
     C*====================================================================
     C* EMP_NAME - GET EMPLOYER NAME
     C*====================================================================
     C*
     C     EMP_NAME      BEGSR
     C*
     C     emky          chain     PEM01                              60
     C*
     C                   if        *in60 = *on
     C                   clear                   EMNAME
     C                   endif                                                  *in60=*on
     C*
     C     elky          setgt     PEL01
     C     elky2         readpe    PEL01                                  60
     C*
     C                   if        *in60 = *on
     C                   clear                   x8pfdt_usa
     C                   else
     C     *cymd         move      ELPFDT        x8pfdt_usa
     C                   endif                                                  *in60=*on
     C*
     C                   ENDSR                                                  EMP_NAME
     C*
     C*====================================================================
     C* PREV_BAL - Calculate previous balance
     C*====================================================================
     C*
     C     PREV_BAL      BEGSR
     C*
     C     asky1         chain     PAS01                              60
     C*
     C                   dow       *in60 = *off
     C                               and ASSTDT < x5stdt
     C                   eval      agt_prev = agt_prev + ASBAL
     C     asky1         reade     PAS01                                  60
     C                   enddo                                                  *in60=*off
     C*
     C                   eval      agt_due = agt_prev + agt_net
     C*
     C                   ENDSR                                                  PREV_BAL
     C*
     C*====================================================================
     C* PROD_BRK - Producer break
     C*====================================================================
     C*
     C     PROD_BRK      BEGSR
     C*
     C                   if        svprod <> *zeros
     C                   exsr      PRT_PROD
     C                   except    PROD_TOTAL
     C                   endif                                                  svprod<>*zeros
     C*
     C                   if        break_lvl = 'P'
     C*
     C                   if        *inob = *on
     C                   exsr      PRT_PROD
     C                   else
     C                   except    HEAD_PROD
     C                   endif                                                  *inob=*on
     C*
     C                   endif                                                  break_lvl=P
     C*
     C                   exsr      PROD_NAME
     C*
     C                   exsr      EMP_BRK
     C*
     C                   eval      svprod = ABPROD
     C*
     C                   if        break_lvl = 'P'
     C                   exsr      PRT_PROD
     C                   except    HEAD_EMP
     C                   endif                                                  break_lvl=P
     C*
     C                   eval      agt_amt  = agt_amt  + prod_amt
     C                   eval      agt_comm = agt_comm + prod_comm
     C                   eval      agt_net  = agt_net  + prod_net
     C*
     C                   clear                   prod_amt
     C                   clear                   prod_comm
     C                   clear                   prod_net
     C*
     C                   ENDSR
     C*
     C*====================================================================
     C* PROD_NAME - GET THE PRODUCER NAME
     C*====================================================================
     C*
     C     PROD_NAME     BEGSR
     C*
     C                   clear                   APNAME
     C*
     C     apky          chain     PAP01                              61
     C*
     C                   if        *in61 = *off
     C     APLSN         cat       ', ':0        APNAME
     C                   cat       APFSN:1       APNAME
     C                   endif
     C*
     C                   ENDSR                                                  PROD_NAME
     C*
     C*====================================================================
     C*  PROCESS - PROCESS RECORDS
     C*====================================================================
     C*
     C     PROCESS       BEGSR
     C*
     C                   if        ABAGT# <> svagt#
     C                   eval      break_lvl = 'A'
     C                   exsr      AGT_BRK
     C                   endif                                                  ABAGT# <> svagt#
     C*
     C                   if        ABPROD <> svprod
     C                   eval      break_lvl = 'P'
     C                   exsr      PROD_BRK
     C                   endif                                                  ABPROD <> svprod
     C*
     C                   if        ABEMP# <> svemp#
     C                               or ABDIV <> svdiv
     C                               or ABFND <> svfnd
     C                   eval      break_lvl = 'E'
     C                   exsr      EMP_BRK
     C                   endif                                                  ABPROD <> svprod
     C*
     C                   if        *inob = *on
     C                   exsr      PRT_HEAD
     C                   eval      *inob = *off
     C                   endif                                                  *inob=*on
     C*
     C                   select
     C***  BEGIN ADD  ***  07/02/01  **************************************
     C***  BEGIN ADD  ***  08/23/06  **************************************
     C**********         when      ABARTY = 'P'
     C*******************when******ABARTY = 'P' or ABARTY = 'Q' or ABARTY = 'I'
     C                   when      ABARTY = 'P' or ABARTY = 'Q' or ABARTY = 'I'
     C                                 or ABARTY = 'T'
     C***  END   ADD  ***  08/23/06  **************************************
     C***  END   ADD  ***  07/02/01  **************************************
     C                   eval      prod_net  = prod_net  - ABAMT
     C                   mult      -1            ABAMT
     C                   except    DETAIL_PMT
     C***  Begin Add  ***  07/06/06  **************************************
     C                   when      ABARTY = 'R' or ABARTY = 'V'
     C                   eval      prod_net  = prod_net  + ABAMT
     C                   except    DETAIL_PMT
     C***  End   Add  ***  07/06/06  **************************************
     C                   other
     C                   eval      prod_amt  = prod_amt  + ABAMT
     C                   eval      prod_comm = prod_comm + ABCOMM
     C                   eval      prod_net  = prod_net  + ABNET
     C                   except    DETAIL
     C                   endsl
     C*
     C                   ENDSR                                                  PROCESS
     C*
     C*====================================================================
     C* PRT_HEAD - PRINT HEADERS
     C*====================================================================
     C*
     C     PRT_HEAD      BEGSR
     C*
     C                   except    HEAD
     C                   except    HEAD_AGT
     C*
     C                   if        ABPROD <> *zeros
     C                   except    HEAD_PROD
     C                   endif                                                  ABPROD<>*zeros
     C*
     C                   if        ABEMP# <> *zeros
     C                   except    HEAD_EMP
     C                   endif                                                  ABPROD<>*zeros
     C*
     C                   eval      *inob = *off
     C*
     C                   ENDSR
     C*
     C*====================================================================
     C* PRT_PROD - PRINT PRODUCER HEADERS
     C*====================================================================
     C*
     C     PRT_PROD      BEGSR
     C*
     C                   if        *inob = *on
     C                   except    HEAD
     C                   except    HEAD_AGT
     C                   except    HEAD_PROD
     C                   eval      *inob = *off
     C                   endif                                                  *inob=*on
     C*
     C                   ENDSR                                                  PRT_PROD
     C*____________________________________________________________________
     O*
     O/COPY SCOPYROUT
     O
     O***  Begin Chg  ***  08/18/14  **************************************
     O**********E            ROUTE          2 33
     O          E            ROUTE       2  2
     O***  End   Chg  ***  08/18/14  **************************************
     O                                           23 'REPORT SELECTIONS:'
     O          E            ROUTE          2
     O                                           26 'COMPANY  . . . . . :'
     O                       xxco#         Z   +002
     O          E            ROUTE          2
     O                                           26 'STATMENT MONTH/YEAR:'
     O                       x6stdt            +002 '  /    '
     O          E            ROUTE          2
     O                                           26 'AGENT NUMBER . . . :'
     O               20                        +002 '*ALL'
     O              N20      xxagt#        Z     33
     O*** Beg Chg *****  01/28/10  ************************************
     O***       E            ROUTE            60
     O***                                        23 '(C) Copyright Progressi'
     O***                                      +  0 've Data Solutions, Inc.,'
     O          e            route          1 59
     O                       #brnd1
     O          e            route          1
     O                       #brnd2
     O*** End Chg *****  01/28/10  ************************************
     OQSYSPRT   E            HEAD           1  6
     O                       dstime              10 '  :  :  '
     O                       CONAME              82
     O***** BEGIN CHG **** 01/07/00 ****************************************
     O**********             dsdate        y    123
     O                       dsdate        y    120
     O***** END   CHG **** 01/07/00 ****************************************
     O          E            HEAD           2
     O                       q1pgm               12
     O                       ortitl              92
     O***** BEGIN CHG **** 01/07/00 ****************************************
     O**********                                117 'PAGE:'
     O**********             page          z    123
     O                                          114 'PAGE:'
     O                       page          z    120
     O***** END   CHG **** 01/07/00 ****************************************
     O          E            HEAD           0
     O***** BEGIN CHG **** 01/07/00 ****************************************
     O**********                                112 'STATEMENT DATE:'
     O**********             x6stdt             123 '  /    '
     O                                          109 'STATEMENT DATE:'
     O                       x6stdt             120 '  /    '
     O***** END   CHG **** 01/07/00 ****************************************
     O          E            HEAD_AGT       1
     O***** BEGIN CHG **** 04/04/01 ****************************************
     O**********             ABAGT#        Z     19
     O                       svAGT#        Z     19
     O**********             AGNAME              60
     O                       xxNAME              60
     O***** END   CHG **** 04/04/01 ****************************************
     O          E            HEAD_AGT       1
     O***** BEGIN CHG **** 04/04/01 ****************************************
     O**********             AGMAD1              60
     O                       xxMAD1              60
     O***** END   CHG **** 04/04/01 ****************************************
     O***** BEGIN CHG **** 01/07/00 ****************************************
     O**********                                112 'DUE DATE:'
     O**********             x8due_usa          123
     O                                          109 'DUE DATE:'
     O***** BEGIN CHG **** 04/04/01 ****************************************
     O**********             x8due_usa          120
     O                       xxdue_dte          120
     O***** END   CHG **** 04/04/01 ****************************************
     O***** END   CHG **** 01/07/00 ****************************************
     O          E            HEAD_AGT       2
     O***** BEGIN CHG **** 04/04/01 ****************************************
     O**********             AGCSZ               60
     O                       xxCSZ               60
     O***** END   CHG **** 04/04/01 ****************************************
     O          E            HEAD_AGT       1
     O                                            9 'GRP'
     O                                         +001 ' EMPLOYER'
     O                                         +001 '  DIV'
     O                                         +002 'NAME'
     O***** BEGIN CHG **** 01/07/00 ****************************************
     O*********                                +040 'POL EFF DT'
     O                                         +037 'POL EFF DT'
     O***** END   CHG **** 01/07/00 ****************************************
     O          E            HEAD_AGT       2
     O                                           22 ' YR'
     O                                         +002 'CD'
     O                                         +001 'TP'
     O                                         +001 'DESC'
     O***** BEGIN CHG **** 01/07/00 ****************************************
     O**********                                 79 'AMOUNT'
     O**********                               +006 'RATE'
     O**********                                104 'COMMISSION AMT'
     O**********                                121 'NET AMOUNT'
     O                                           76 'AMOUNT'
     O                                         +006 'RATE'
     O                                          101 'COMMISSION AMT'
     O                                          118 'NET AMOUNT'
     O***** END   CHG **** 01/07/00 ****************************************
     O          E            HEAD_PROD      1
     O                                           15 'PRODUCER:'
     O                       ABPROD        Z     21
     O                       APNAME            +002
     O***  BEGIN CHG  ***  11/19/99  **************************************
     O**********E            HEAD_EMP    1
     O          E            HEAD_EMP    1  1
     O***  END   CHG  ***  11/19/99  **************************************
     O                       ABFND         Z      9
     O                       ABEMP#        Z   +001
     O                       ABDIV         Z   +001
     O                       EMNAME            +002
     O***  BEGIN CHG  ***  11/19/99  **************************************
     O**********             x8pfdt_usa        +004
     O                       x8pfdt_usa        +001
     O***  END   CHG  ***  11/19/99  **************************************
     O          E            DETAIL_PMT     1
     O                       ABFYR         Z     22
     O                       ABARCD            +002
     O                       ABARTY            +001
     O                       ABDESC            +002
     O***** BEGIN CHG **** 01/07/00 ****************************************
     O**********             ABAMT         A    123
     O                       ABAMT         A    120
     O***** END   CHG **** 01/07/00 ****************************************
     O***  BEGIN CHG  ***  11/19/99  **************************************
     O**********E            DETAIL      1
     O          E            DETAIL         1
     O***  END   CHG  ***  11/19/99  **************************************
     O                       ABFYR         Z     22
     O                       ABARCD            +002
     O                       ABARTY            +001
     O                       ABDESC            +001
     O***** BEGIN CHG **** 01/07/00 ****************************************
     O**********             ABAMT         A     81
     O**********             ABRATE        K   +002
     O**********             ABCOMM        A    106
     O**********             ABNET         A    123
     O                       ABAMT         A     78
     O                       ABRATE        K   +002
     O                       ABCOMM        A    103
     O                       ABNET         A    120
     O***** END   CHG **** 01/07/00 ****************************************
     O          E            PROD_TOTAL  2  1
     O                                           55 'PRODUCER TOTAL:'
     O***** BEGIN CHG **** 01/07/00 ****************************************
     O**********             prod_amt      A     81
     O**********             prod_comm     A    106
     O**********             prod_net      A    123
     O                       prod_amt      A     78
     O                       prod_comm     A    103
     O                       prod_net      A    120
     O***** END   CHG **** 01/07/00 ****************************************
     O          E            AGT_TOTAL   1  1
     O                                           55 'AGENT TOTAL   :'
     O***** BEGIN CHG **** 01/07/00 ****************************************
     O**********             agt_amt       A     81
     O**********             agt_comm      A    106
     O**********             agt_net       A    123
     O                       agt_amt       A     78
     O                       agt_comm      A    103
     O                       agt_net       A    120
     O***** END   CHG **** 01/07/00 ****************************************
     O          E            AGT_TOTAL      0
     O                                           55 'PREVIOUS BAL  :'
     O***** BEGIN CHG **** 01/07/00 ****************************************
     O**********             agt_prev      A    123
     O                       agt_prev      A    120
     O***** END   CHG **** 01/07/00 ****************************************
     O          E            AGT_TOTAL      1
     O***** BEGIN CHG **** 01/07/00 ****************************************
     O**********                                121 '______________'
     O                                          118 '______________'
     O***** END   CHG **** 01/07/00 ****************************************
     O          E            AGT_TOTAL      0
     O                                           55 'TOTAL DUE     :'
     O***** BEGIN CHG **** 01/07/00 ****************************************
     O**********             agt_due       A    123
     O                       agt_due       A    120
     O***** END   CHG **** 01/07/00 ****************************************
     O          E            TOTAL       2  1
     O***** BEGIN CHG **** 01/07/00 ****************************************
     O**********                                 79 'Premium'
     O**********                                104 'Commission Amt'
     O**********                                121 'Net Amount Due'
     O                                           76 'Premium'
     O                                          101 'Commission Amt'
     O                                          118 'Net Amount Due'
     O***** END   CHG **** 01/07/00 ****************************************
     O          E            TOTAL          1
     O                                           55 'TOTAL       :'
     O***** BEGIN CHG **** 01/07/00 ****************************************
     O**********             total_amt     A     81
     O**********             total_comm    A    106
     O**********             total_net     A    123
     O                       total_amt     A     78
     O                       total_comm    A    103
     O                       total_net     A    120
     O***** END   CHG **** 01/07/00 ****************************************
     O          E            TOTAL          0
     O                                           55 'PREVIOUS BAL:'
     O***** BEGIN CHG **** 01/07/00 ****************************************
     O**********             total_prev    A    123
     O                       total_prev    A    120
     O***** END   CHG **** 01/07/00 ****************************************
     O          E            TOTAL          1
     O***** BEGIN CHG **** 01/07/00 ****************************************
     O**********                                121 '______________'
     O                                          118 '______________'
     O***** END   CHG **** 01/07/00 ****************************************
     O          E            TOTAL          0
     O                                           55 'TOTAL DUE   :'
     O***** BEGIN CHG **** 01/07/00 ****************************************
     O**********             total_due     A    123
     O                       total_due     A    120
     O***** END   CHG **** 01/07/00 ****************************************
     O          E            TTL_NOREC   2
     O                                              '* * * NO RECORDS MATCH '
     O                                         +000 'SELECTION CRITERIA * * *'
