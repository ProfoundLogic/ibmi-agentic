     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     h option(*nodebugio: *srcstmt)
     h debug(*yes)
     H*****************************************************************
     H*  Created:                                                     *
     H*  01/16/98  C:69  Project: WC753                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 03/20/25 C:342 Support: 118678                                *
     H*                - Delete emp# = 0 record from wdeap when       *
     H*                  record added by A/R Batch Detail program and *
     H*                  pressing F3/exit.                            *
     H*                                                               *
     H* 11/06/24 C:342 Support: 117962                                *
     H*                1.Delete record from Wxrip while exit.         *
     H*                2.populate user in Wxrip file.                 *
      *                3.do not populate fnd# and emp# in Wxrip file. *
     H*                                                               *
      * 05/24/21 C:180 SUPPORT 95396                                  *
      *                Prevent batch from being deleted while in use. *
     H*                                                               *
     H* 07/17/17 C:65  PROJECT: 150WAP                                *
     H*                ADD CODING TO RECOGNIZE THE 'C' VALUE FOR PEO. *
     H*                                                               *
     H* 01/13/11 C:257 Project: WC1948                                *
     H*                Do not allow option 4 if either the flag       *
     H*                for approved or the flag for transfer          *
     H*                is set to "Y"                                  *
     H*                                                               *
     H* 01/04/10 C:190 PROJECT: 25WRE                                 *
     H*                Created new subfile view to display            *
     H*                statement# (EAINV#) as first column            *
     H*                for clt025.                                    *
     H*                                                               *
     H* 06/17/08 C:189 Project: SY156                                 *
     H*                Corrected the Positioning issue of the Amount  *
     H*                field                                          *
     H*                                                               *
     H* 04/29/08 C:61  Project: 71WIY                                 *
     H*               - Convert to RPGIV.  Conversion changes not     *
     H*                 commented.                                    *
     H*               - Change subfile display and add new views      *
     H*                 (view 2 and 3).                               *
     H*               - Add subset screen. (group, emp#, div, fyr,    *
     H*                                     emp name contains...)     *
     H*                                                               *
     H* 08/22/07 C:33  Project: 114WMF                                *
     H*                Compare FNPEOF to 'Y' as well as 'M'.          *
     H*                                                               *
     H* 07/25/05 C:053 PROJECT: WC1279 (CORRECTION)                   *
     H*                Change SYFL12 to WCFL30.                       *
     H*                                                               *
     H* 04/18/05 C:132 PROJECT: WC1297 PEO Module
     H*                Check PEO Install flagg if = "Y" and the       *
     H*                PEO flag in WMFNP and if = "M" bypass          *
     H*                the Edit that makes sure check amount and      *
     H*                accumlated amounts posted for check are equal. *
     H*                                                               *
     H* 03/03/05 C:062 PROJECT: WC1229                                *
     H*                Add Group restriction logic.                   *
     H*                                                               *
     H* 01/16/04 C:132 PROJECT: WC1174                                *
     H*                Add Group number to Crossfoot Error Screen     *
     H*                                                               *
     H* 01/13/03 C:61  SUPPORT WA121 35492                            *
     H*                Correct Edit that makes sure check amount and  *
     H*                accumlated amounts posted for check are equal. *
     H*                                                               *
     H*  09/17/02 C:136 SUPPORT: WC573                                *
     H*                 CROSSFOOT TOTALS NOT CORRECT ON SCR04.        *
     H*                                                               *
     H*  03/08/00 C:33  SEQ#: WA033/22101                             *
     H*                 DO NOT CLEAR SVCKAM, BUT LOAD IT W/EACKAM.    *
     H*                                                               *
     H*  01/31/00 C:33  PROJECT: WC577                                *
     H*                 BE SURE THAT SEPARATE CHECKS ARE CALCULATED   *
     H*                 SEPARATELY.                                   *
     H*                                                               *
     H*  02/25/99 C:69  Support: Call from JJ                         *
     H*                 The delete confirmation screen was not showing*
     H*                 any information. Display was changed to show  *
     H*                 data fields on delete. No changes were        *
     H*                 necessary in RPG. Program just needed to be   *
     H*                 recompiled because of changed display file.   *
     H*                                                               *
     H*  08/11/98 C:69  Support: 13116                                *
     H*                 Added condition so that zero checks would not *
     H*                 be included in check x-foot errors.           *
     H*                                                               *
     H*  05/05/98  C:33  SEQ#: WA091/11440                            *
     H*                  ALLOW FOR MULTIPLE EMPLOYERS WITH THE SAME   *
     H*                  CHECK #.                                     *
     H*                                                               *
     H*  02/25/98  C:69  Supp Seq: 10439                              *
     H*                  Corrected paging problem when there more than*
     H*                  one page of items for the same check.        *
     H*****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW1210F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     SFILE(SCR04S:SFRRN4)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWDEAP     IF   E             DISK
     F                                     RENAME(PEA01:PFRCDF)
     F*
     F*** BEG ADD *** 04/18/05 ************************************    **
     F*  Group Master file
      ***** BEG chg ******* 04/29/08 **********************************
     F*******WMFNL     IF   E           K DISK
     fwmfnl     if   e           k disk    infds(fnfds)
      ***** END chg ******* 04/29/08 **********************************
     F*** BEG CHG *** 07/25/05 ************************************    **
     F*SSYPF   IF  E                    DISK
     FSSYLF     IF   E           K DISK
     F*** END CHG *** 07/25/05 ************************************    **
     F*** END ADD *** 04/18/05 ************************************    **
      ***** beg del ******* 04/29/08 **********************************
     F********WMEML     IF   E           K DISK
      ***** end del ******* 04/29/08 **********************************
      ***** BEG ADD ******* 04/29/08 **********************************
     fWMEML     if   e           k disk    infds(emfds)
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM02)

      * Employer Policy Year
     FWDELP     IF   E             DISK
      ***** END ADD ******* 04/29/08 **********************************

     F*  Primary logical file definition (for displaying items)
     FWDEAL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PEA01:DBRCDF)
      ***** BEG ADD ******* 04/29/08 **********************************

      *  View #2
     FWDEALE    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PEA01:DBRCDF2)

      *  View #3
     FWDEALD    IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PEA01:DBRCDF3)

      ***** END ADD ******* 04/29/08 **********************************
      ***** BEG ADD ***** 01/04/10 ************************************
      *  View #4
     FWDEAL10   IF   E           K DISK
     F                                     INFDS(DBFDS4)
     F                                     RENAME(PEA01:DBRCDF4)

      ***** END ADD ******* 01/04/10 **********************************
     F*** BEGIN ADD *** 03/03/05 **************************************
     FWMUPL     IF   E           K DISK
     F*** END   ADD *** 03/03/05 **************************************
      //*** Begin Add *** 05/24/21 *****************************************
       Dcl-F WXRIL3 disk usage(*update:*delete:*output) keyed;
       Dcl-Ds KeyWXRIL3 likerec(PRI01:*KEY);
      //**  End   Add *** 05/24/21 *****************************************
     F**** Beg Add ***** 03/20/25 ***********************************
     Fwdeal4    if   e           k disk    rename(pea01:pea04)
     F                                     prefix(L4_)
     F**** End Add ***** 03/20/25 ***********************************
     D*
      ***** BEG DEL ******* 04/29/08 **********************************
     D*HD******        S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D*OF******        S              1    DIM(11) CTDATA PERRCD(1)             Options, Function
     D*OFD*****        S             35    DIM(11) ALT(OF)
      ***** END DEL ******* 04/29/08 **********************************
      ***** BEG ADD ******* 04/29/08 **********************************

     d emfds           ds
     d   emrrn               397    400B 0

     d fnfds           ds
     d   fnrrn               397    400B 0

      ***** BEG CHG ***** 01/04/10 ************************************
     D HD              S             78    DIM(4) CTDATA PERRCD(1)              Column headings
     D**HD*******       S             78    DIM(3) CTDATA PERRCD(1)              Column headings
      ***** END CHG ***** 01/04/10 ************************************
     D OF              S              1    DIM(12) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(12) ALT(OF)
     D elrrn           s              9  0 inz
     D inkc            s                   like(*inkc)
     D inkl            s                   like(*inkl)
     D err             s              1    inz
      ***** END ADD ******* 04/29/08 **********************************
      *
     D                 DS
     D  OFDATA                01     35
     D  MAINTF                28     28
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('A/R Batch Detail')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W1210R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
      ***** BEG CHG ******* 04/29/08 **********************************
     D*VIEWMX********* C                   CONST(1)
      ***** BEG DLT ***** 01/04/10 ************************************
     D******* VIEWMX          C                   CONST(3)
      ***** END DLT ***** 01/04/10 ************************************
     D lo              C                   'abcdefghijklmnopqrs-
     D                                     tuvwxyz'
     D up              C                   'ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ'
      ***** END CHG ******* 04/29/08 **********************************
     D SF1PAG          C                   CONST(8)
     D**** Beg Add ***** 03/20/25 ***********************************
     d count           s              3  0
     D**** End Add ***** 03/20/25 ***********************************
      *
     C/COPY SCOPYWW01C
      *
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0
     C*** BEGIN ADD *** 03/03/05 **************************************
     C                   PARM                    @@GRST            1
     C*** END   ADD *** 03/03/05 **************************************
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@RRN         ##BHDR            9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY           KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSTAT
     C                   KFLD                    KKBAT#
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSTAT
     C                   KFLD                    KKBAT#
     C                   KFLD                    KKCHK#
     C                   KFLD                    KKCKAM
     C                   KFLD                    KKDT
     C                   KFLD                    KKTIME
      ***** BEG ADD ******* 04/29/08 **********************************

     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSTAT
     C                   KFLD                    KKBAT#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
     C                   KFLD                    KKDT
     C                   KFLD                    KKTIME

     C     KEY3          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSTAT
     C                   KFLD                    KKBAT#
     C                   KFLD                    KKCKAM
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
     C                   KFLD                    KKDT
     C                   KFLD                    KKTIME

      ***** END ADD ******* 04/29/08 **********************************
      ***** BEG ADD ******* 01/04/10 **********************************
     C     KEY4          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSTAT
     C                   KFLD                    KKBAT#
     C                   KFLD                    KKINV#
     C                   KFLD                    KKCHK#
     C                   KFLD                    KKCKAM
     C                   KFLD                    KKDT
     C                   KFLD                    KKTIME
      ***** END ADD ******* 01/04/10 **********************************
     C*
     C     KEYCHK        KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSTAT
     C                   KFLD                    KKBAT#
     C                   KFLD                    KKCHK#
     C                   KFLD                    KKCKAM
     C*
     C     BATKEY        KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASTAT
     C                   KFLD                    EABAT#
     C*
     C     EMKEY         KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    EAEMP#
     C                   KFLD                    EADIV
      ***** BEG ADD ******* 04/29/08 **********************************
      *
     C     EMKY1         KLIST
     C                   KFLD                    K1CO#             3 0
     C                   KFLD                    K1FND             3 0
     C                   KFLD                    K1EMP#            9 0
     C                   KFLD                    K1DIV             5 0
      ***** END ADD ******* 04/29/08 **********************************
     C*** BEG ADD *** 04/18/05 ************************************    **
     C*  Group Master file Key
     C     FNKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C*** END ADD *** 04/18/05 ************************************    **
      ***** BEG ADD ******* 04/29/08 **********************************
     C     FNKY1         KLIST
     C                   KFLD                    K1CO#             3 0
     C                   KFLD                    K1FND             3 0
      ***** END ADD ******* 04/29/08 **********************************


     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    EACO#         KKCO#
     C     *LIKE         DEFINE    EASTAT        KKSTAT
     C     *LIKE         DEFINE    EABAT#        KKBAT#
     C     *LIKE         DEFINE    EACHK#        KKCHK#
     C     *LIKE         DEFINE    EACKAM        KKCKAM
     C     *LIKE         DEFINE    EADT          KKDT
     C     *LIKE         DEFINE    EATIME        KKTIME
      ***** BEG ADD ******* 04/29/08 **********************************
     C     *LIKE         DEFINE    EAFND         KKFND
     C     *LIKE         DEFINE    EAEMP#        KKEMP#
     C     *LIKE         DEFINE    EADIV         KKDIV
     C     *LIKE         DEFINE    EAFYR         KKFYR
      ***** END ADD ******* 04/29/08 **********************************
      ***** BEG ADD ******* 01/04/10 **********************************
     C     *LIKE         DEFINE    EAINV#        KKINV#
      ***** END ADD ******* 01/04/10 **********************************
     C*** BEGIN ADD *** 03/03/05 **************************************
     C     *LIKE         DEFINE    *IN99         MULTIG
     C     *LIKE         DEFINE    *IN99         FAILED
     C*** END   ADD *** 03/03/05 **************************************
      *
      *
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
      ***** BEG ADD ******* 04/29/08 **********************************

     c                   if        (ssfnd <> 0)
     c                             or (ssemp# <> 0)
     c                             or (ssdiv <> 0)
     c                             or (ssfyr <> 0)
     c                             or (sschk# <> 0)
     c                             or (ssemnm <> *blanks)
      ***** BEG ADD ******* 01/04/10 **********************************
     c                             or (ssinv# <> 0)
      ***** END ADD ******* 01/04/10 **********************************
     c                   eval      *in11 = *on
     c                   endif

      ***** END ADD ******* 04/29/08 **********************************
     C                   ENDSR


     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*** BEGIN ADD *** 03/03/05 **************************************
     C     Q1PRMS        IFGT      4
     C     SCSEL         IFEQ      '2'
     C     SCSEL         OREQ      '4'
     C     SCGRST        IFEQ      'Y'
     C     SCGRST        OREQ      'I'
     C                   SETON                                        7190
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 03/03/05 **************************************
     C*** BEGIN ADD *** 01/13/11 **************************************
     C                   if        (scsel = '4' and eatffl = 'Y')
     C                             or (scsel = '4' and eatfap = 'Y')
     C                   seton                                        6590
     C                   endif
     C*** END   ADD *** 01/13/11 **************************************
     C                   ENDSR


     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
      ***** BEG ADD ******* 04/29/08 **********************************

     C     *inkj         caseq     *on           kjdo                     90
     C                   endcs

      ***** END ADD ******* 04/29/08 **********************************
     C                   ENDSR


     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*** BEGIN ADD *** 03/03/05 **************************************
     C                   SETOFF                                       16
     C                   CLEAR                   SSGRST
     C     Q1PRMS        IFGT      4
     C     @@GRST        ANDEQ     'Y'
     C                   MOVEL     'N'           SSGRST
     C                   SETON                                        16
     C                   ENDIF
     C*** END   ADD *** 03/03/05 **************************************
      ***** BEG ADD ******* 04/29/08 **********************************
     c                   eval      ssfnd  = 0
     c                   eval      ssemp# = 0
     c                   eval      ssdiv  = 0
     c                   eval      ssfyr  = 0
     c                   eval      sschk# = 0
     c                   eval      ssemnm = *blanks
      ***** END ADD ******* 04/29/08 **********************************
      ***** BEG ADD ******* 01/04/10 **********************************
     c                   eval      ssinv# = 0
      ***** END ADD ******* 01/04/10 **********************************
     C                   ENDSR


     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVE      *LOVAL        KKCHK#
     C                   MOVE      *LOVAL        KKCKAM
     C                   MOVE      *LOVAL        KKDT
     C                   MOVE      *LOVAL        KKTIME
      ***** BEG ADD ******* 04/29/08 **********************************
     C                   MOVE      *LOVAL        KKFND
     C                   MOVE      *LOVAL        KKEMP#
     C                   MOVE      *LOVAL        KKDIV
     C                   MOVE      *LOVAL        KKFYR
      ***** END ADD ******* 04/29/08 **********************************
      ***** BEG ADD ******* 01/04/10 **********************************
     C                   MOVE      *LOVAL        KKINV#
      ***** END ADD ******* 01/04/10 **********************************
     C                   ENDSR


     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
      ***** BEG DEL ******* 04/29/08 **********************************
     C*********          MOVE      SCPST1        KKCHK#
     C*********          MOVE      *LOVAL        KKCKAM
     C*********          MOVE      *LOVAL        KKDT
     C*********          MOVE      *LOVAL        KKTIME
      ***** END DEL ******* 04/29/08 **********************************
      ***** BEG ADD ******* 04/29/08 **********************************

     c                   select

     c                   when      *in21 = *on
     c                   MOVE      SCPST1        KKCHK#
     c                   MOVE      *LOVAL        KKCKAM

     c                   when      *in22 = *on
     c                   MOVE      SCPST2        KKFND
     c                   MOVE      SCP2EM        KKEMP#
     c                   MOVE      SCP2DV        KKDIV
     c                   MOVE      *LOVAL        KKFYR
      *
     c                   when      *in23 = *on
      ***** BEG CHG ******* 06/17/08 **********************************
     c*************      move      SCPST3        KKCKAM
     c                   movel     SCPST3        ##PST3            9 0
     c                   Eval      KKCKAM   =##PST3
      ***** END CHG ******* 06/17/08 **********************************
     c                   move      SCP3FN        KKFND
     c                   move      SCP3EM        KKEMP#
     c                   move      SCP3DV        KKDIV
     c                   MOVE      *LOVAL        KKFYR
      ***** BEG ADD ******* 01/04/10 **********************************
     c                   when      *in24 = *on
     c                   MOVE      SCPST4        KKinv#
     c                   MOVE      *LOVAL        KKCHK#
     c                   MOVE      *LOVAL        KKCKAM
      ***** END ADD ******* 01/04/10 **********************************

     c                   endsl

     c                   MOVE      *LOVAL        KKDT
     c                   MOVE      *LOVAL        KKTIME
      *
      ***** END ADD ******* 04/29/08 **********************************
     C                   ENDSR


     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVE      SCCHK#        KKCHK#
     C                   MOVE      SCCKAM        KKCKAM
     C                   MOVE      SCDT          KKDT
     C                   MOVE      SCTIME        KKTIME
      ***** BEG ADD ******* 04/29/08 **********************************
     C                   MOVE      SCFND         KKFND
     C                   MOVE      SCEMP#        KKEMP#
     C                   MOVE      SCDIV         KKDIV
     C                   MOVE      SCFYR         KKFYR
      ***** END ADD ******* 04/29/08 **********************************
      ***** BEG ADD ******* 01/04/10 **********************************
     C                   MOVE      SCINV#        KKINV#
      ***** END ADD ******* 01/04/10 **********************************
     C                   ENDSR
      ***** BEG ADD ******* 04/29/08 **********************************


     *================================================================
     * KJDO - Search Subroutine for F10
     *================================================================
     c     KJDO          begsr

     c                   select

      ** Group
     c                   when      hfld = 'SSFND'
     c                   move      ##co#         #1CO#             3

     c                   call      'W0010R'
     c                   parm      *blank        #1kc              1
     c                   parm      *blank        #1kl              1
     c                   parm      '2'           #1call            1
     c                   parm                    #1co#             3
     c                   parm      *blanks       #1fnd             3

     c                   if        #1fnd <> *blanks
     c                   move      ##co#         k1co#
     c                   move      #1fnd         k1fnd
     c     fnky1         chain     pfn01
     c                   eval      ssfnd = fnfnd
     c                   eval      *in72 = *on
     c                   else
     c                   eval      *in71 = *on
     c                   endif

      *** Employer
     c                   when      hfld = 'SSEMP#' or hfld = 'SSDIV'
     c                   move      ##co#         k1co#
     c                   move      ssfnd         k1fnd
     c     fnky1         chain     pfn01

     c                   if        not %found
     c                   z-add     *zero         fnrrn
     c                   endif

     c                   call      'W0050R'
     c                   parm      *blank        #2kc              1
     c                   parm      *blank        #2kl              1
     c                   parm      '2'           #2call            1
     c     emrrn         parm      *zero         #2rrn             9 0
     c                   parm      fnrrn         #2rrn2            9 0
     c                   parm      *blank        #2qte             1

     c                   if        emrrn <> *zero
     c     emrrn         chain     wmemp
     c                   eval      ssfnd  = emfnd
     c                   eval      ssemp# = ememp#
     c                   eval      ssdiv  = emdiv
     c                   eval      *in74 = *on
     c                   else
     c                   if        hfld = 'SSEMP#'
     c                   eval      *in72 = *on
     c                   endif
     c                   if        hfld = 'SSDIV'
     c                   eval      *in73 = *on
     c                   endif
     c                   endif

      *** Employer Policy Year
     c                   when      hfld = 'SSFYR'
     c                   move      ##co#         k1co#
     c                   move      ssfnd         k1fnd
     c                   move      ssemp#        k1emp#
     c                   move      ssdiv         k1div
     c     emky1         chain     pem01

     c                   if        not %found
     c                   z-add     *zero         emrrn
     c                   move      ##co#         k1co#
     c                   move      ssfnd         k1fnd
     c     fnky1         chain     pfn01

     c                   if        not %found
     c                   z-add     *zero         fnrrn
     c                   endif

     c                   call      'W0050R'
     c                   parm      *blank        #2kc              1
     c                   parm      *blank        #2kl              1
     c                   parm      '2'           #2call            1
     c     emrrn         parm      *zero         #2rrn             9 0
     c                   parm      fnrrn         #2rrn2            9 0
     c                   parm      *blank        #2qte             1

     c                   endif

     c                   if        emrrn <> *zero

     C                   call      'W0051R'
     C                   parm      *blank        #3kc              1
     C                   parm      *blank        #3kl              1
     C                   parm      '2'           #3call            1
     C     elrrn         parm      *zero         #3rrn             9 0
     C                   parm      emrrn         #3rrn2            9 0
     C                   parm      *blank        #3qte             1
      *
     c                   if        elrrn <> *zero
     c     elrrn         chain     wdelp
     c                   eval      ssfnd  = elfnd
     c                   eval      ssemp# = elemp#
     c                   eval      ssdiv  = eldiv
     c                   eval      ssfyr  = elfyr
     c                   eval      *in75 = *on
     c                   endif

     c                   endif

     c                   if        *in75 = *off
     c                   eval      *in74 = *on
     c                   endif

     c                   endsl

     c                   endsr
      ***** END ADD ******* 04/29/08 **********************************


     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   MOVE      EADT          SCDT
     C                   MOVE      EATIME        SCTIME
     C                   MOVE      EACHK#        SCCHK#
     C                   MOVE      EACKAM        SCCKAM
     C                   MOVE      EAAMT         SCAMT

     C                   Z-ADD     EAPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPODT

     C     EAARCD        CAT(P)    EADESC:1      SCDESC
     C                   MOVE      EAFND         SCFND
     C                   MOVE      EAFYR         SCFYR
     C     EMKEY         CHAIN     WMEML                              99

B01  C     *IN99         IFEQ      *OFF
     C                   CALL      'WA005R'                             99
     C                   PARM                    EAEMP#
     C                   PARM                    EADIV
     C                   PARM                    EMNAME
     C                   PARM                    @@EMI            80
     C                   MOVEL(P)  @@EMI         SCEMP
      ***** beg add ******* 04/29/08 **********************************
     c                   eval      scenam = emname + emnam2
      ***** end add ******* 04/29/08 **********************************
E01  C                   ENDIF
      ***** BEG ADD ******* 04/29/08 **********************************

     c                   z-add     eaemp#        scemp#
     c                   z-add     eadiv         scdiv
      ***** BEG ADD ******* 01/04/10 **********************************
     c                   z-add     eainv#        scinv#
      ***** END ADD ******* 01/04/10 **********************************

     c                   select

     c                   when      *in21 = *on
     c                   eval      scdata = ' ' + %editc(scchk#:'Z') +
     c                                      ' '  +
     c                                      %editc(scckam:'J') +
     c                                      '  '  +
     c                                      %editc(scamt:'J') +
     c                                      '  '  +
     c                                      %editw(scpodt:'  /  /    ') +
     c                                      ' '  +
     c                                      SCDESC
     c                   eval      scdat2 = 'Group: ' + %editc(scfnd:'Z') +
     c                                      ' Yr: ' + %editc(scfyr:'Z') +
     c                                      ' Employer: ' + SCEMP

     c                   when      *in22 = *on
     c                   eval      scdata = %editc(scfnd:'Z') +
     c                                      ' ' +
     c                                      %editc(scemp#:'Z') +
     c                                      ' ' +
     c                                      %editc(scdiv:'Z') +
     c                                      '  ' +
     c                                      scenam
     c                   eval      scdat2 = 'Chk: ' +
     c                                       %editc(scchk#:'Z') +
     c                                       ' ' +
     c                                       %editc(scckam:'J') +
     c                                       ' Amt: ' +
     c                                       %editc(scamt:'J') +
     c                                       '  A/R: ' +
     c                                       scdesc

     c                   when      *in23 = *on
     c                   eval      scdata = %editc(scckam:'1') +
     c                                      '  ' +
     c                                      %editc(scfnd:'Z') +
     c                                      ' ' +
     c                                      %editc(scemp#:'Z') +
     c                                      ' ' +
     c                                      %editc(scdiv:'Z') +
     c                                      ' ' +
     c                                      %editc(scfyr:'Z') +
     c                                      ' ' +
     c                                      scenam
     c                   eval      scdat2 = 'Chk: ' +
     c                                       %editc(scchk#:'Z') +
     c                                       '   Amt: ' +
     c                                       %editc(scamt:'J') +
     c                                       '   A/R: ' +
     c                                       scdesc
      ***** BEG ADD ***** 01/04/10 ************************************
     c                   when      *in24 = *on
     c                   eval      scdata = %editc(scinv#:'3') +
     c                                      '  '  +
     c                                      %editc(scchk#:'Z') +
     c                                      '  '  +
     c                                      %editc(scckam:'J') +
     c                                      ' '  +
     c                                      %editc(scamt:'J') +
     c**********                            ' '  +
     c                                      %editw(scpodt:'  /  /    ') +
     c                                      '  '  +
     c                                      SCDESC
     c                   eval      scdat2 = 'Group: ' + %editc(scfnd:'Z') +
     c                                      ' Yr: ' + %editc(scfyr:'Z') +
     c                                      ' Employer: ' + SCEMP
      ***** END ADD ***** 01/04/10 ************************************

     c                   endsl

      ***** END ADD ******* 04/29/08 **********************************
     C                   ENDSR                                                  MVDBSC


     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR


     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C                   ENDSR


     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
      //*** Beg Add ***** 03/20/25 ***********************************
        count = 0;
        If Sfrrn1 = *zeros;
           Setll (kkco#:kkbat#) pea04;
           Reade (kkco#:kkbat#) pea04;
           Dow Not %Eof(wdeal4);
              count = count + 1;
              Reade (kkco#:kkbat#) pea04;
           Enddo;
        Endif;
      //*** End Add ***** 03/20/25 ***********************************
B01  C     SFRRN1        IFNE      *ZEROS
      //*** Beg Add ***** 03/20/25 ***********************************
B01  C     Count         ORGT      1
      //*** End Add ***** 03/20/25 ***********************************
     C                   MOVE      *ALL'9'       KKCHK#
     C                   MOVE      *ALL'9'       KKCKAM
     C     KEYCHK        CHAIN     WDEAL                              99
B02  C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRN         SCDBRR
     C                   MOVE      'E'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
E02  C                   ENDIF
E01  C                   ENDIF
     C*
B01  C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C                   SUB       SFRRN4        SFRRN4               44
     C                   SETON                                        43
     C                   WRITE     SCR04C
     C                   SETOFF                                       43
     C                   EXSR      TOTGET
B02  C     SFRRN4        IFGT      *ZEROS
     C*****************  WRITE     SCR04
     C                   EXFMT     SCR04C
     C                   MOVE      *ON           RSTPGM
E02  C                   ENDIF
E01  C                   ENDIF
     C*
          //*** Begin Add *** 05/24/21 ************************************
          //*** Begin Chg *** 11/06/24 ************************************
          //*** Unlock WXRIL3;
           delete PRI01;
          //*** End   Chg *** 11/06/24 ************************************
          //*** End   Add *** 05/24/21 ************************************
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
      *** BEG ADD ***** 01/04/10 ***********************************************
     c                   call      'RTVCLTID'
     c                   parm                    clientID          3
     c                   if        clientid = '025'
     c                   move      '4'           viewmx            1 0
     c                   move      '4'           view#
     c                   else
     c                   move      '3'           viewmx
     c                   move      '1'           view#
     c                   endif
      *** END ADD ***** 01/04/10 ***********************************************
     C*
     C                   SETON                                        12
     C*
     C     @@RRN         CHAIN     WDEAP                              99
B01  C     *IN99         IFEQ      *OFF
     C                   MOVE      EACO#         KKCO#
     C                   MOVE      EASTAT        KKSTAT
     C                   MOVE      EABAT#        KKBAT#
     C                   MOVE      EABAT#        SCBAT#
     C                   MOVE      EABATA        SCBATA
          //*** Begin Add *** 05/24/21 ************************************
           KeyWXRIL3.RInam  = 'WDEAP';
           KeyWXRIL3.RIco#  = EAco#;
           KeyWXRIL3.RIbat# = EAbat#;
           Monitor;
             Chain %kds(KeyWXRIL3) PRI01;         //Lock it
             If not %found(WXRIL3);
               Clear PRI01;
               RIstat = 'A';
               RInam = 'WDEAP';
               RIrrn = @@rrn;
               //*** Beg Del *** 11/06/24 ************************************
               //*** RIfnd = EAfnd;
               //*** End   Del *** 11/06/24 ************************************
               RIco# = EAco#;
               //*** Beg Del *** 11/06/24 ************************************
               //*** RIemp# = EAemp#;
               //*** End   Del *** 11/06/24 ************************************
               RIbat# = EAbat#;
               RIjobn = Q1jobn;
               RIJOB =  Q1job;
               RIdate = Q1dt;
               RITIME = Q1time;
               RIipgm = Q1pgm;
               RIpgm  = Q1pgm;
               //*** Beg Add *** 11/06/24 **************
               RIuser = Q1user;
               //*** End Add *** 11/06/24 **************
               write PRI01;
               Chain %kds(KeyWXRIL3) PRI01;         //Lock it
             Endif;
           On-Error;
           EndMon;
          //*** End   Add *** 05/24/21 ************************************
E01  C                   ENDIF
     C*** BEGIN ADD *** 03/03/05 **************************************
     C     Q1PRMS        IFGT      4
     C     Q1USER        CHAIN     PUP01                              99
     C*
     C                   CALL      'SA008R'
     C                   PARM      ##CO#         $$CO#
     C                   PARM      Q1USER        $$USR
     C                   PARM      'W'           $$APPL
     C     MULTIG        PARM      *OFF          $$RTCD
     C                   ENDIF
     C*** END   ADD *** 03/03/05 **************************************
     C*
     C                   ENDSR


     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
B01  C                   SELECT
S01  C     *IN21         WHENEQ    *ON
     C     KEY           READE     DBRCDF                                 95
E01  C                   ENDSL
      ***** BEG ADD ******* 04/29/08 **********************************
     C   22KEY           READE     DBRCDF2                                95
     C   23KEY           READE     DBRCDF3                                95
      ***** END ADD ******* 04/29/08 **********************************
      ***** BEG ADD ******* 01/04/10 **********************************
     C   24KEY           READE     DBRCDF4                                95
      ***** END ADD ******* 01/04/10 **********************************
     C                   ENDSR


     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY           READPE    DBRCDF                                 95
      ***** BEG ADD ******* 04/29/08 **********************************
     C   22KEY           READPE    DBRCDF2                                95
     C   23KEY           READPE    DBRCDF3                                95
      ***** END ADD ******* 04/29/08 **********************************
      ***** BEG ADD ******* 01/04/10 **********************************
     C   24KEY           READPE    DBRCDF4                                95
      ***** END ADD ******* 01/04/10 **********************************
     C                   ENDSR


     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
      ***** BEG ADD ******* 04/29/08 **********************************
     C   22KEY2          SETGT     DBRCDF2                            96
     C   23KEY3          SETGT     DBRCDF3                            96
      ***** END ADD ******* 04/29/08 **********************************
      ***** BEG ADD ******* 01/04/10 **********************************
     C   24KEY4          SETGT     DBRCDF4                            96
      ***** END ADD ******* 01/04/10 **********************************
     C                   ENDSR


     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
      ***** BEG ADD ******* 04/29/08 **********************************
     C   22KEY2          SETLL     DBRCDF2                            96
     C   23KEY3          SETLL     DBRCDF3                            96
      ***** END ADD ******* 04/29/08 **********************************
      ***** BEG ADD ******* 01/04/10 **********************************
     C   24KEY4          SETLL     DBRCDF4                            96
      ***** END ADD ******* 01/04/10 **********************************
     C                   ENDSR


     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
      ***** BEG ADD ******* 04/29/08 **********************************
     c                   eval      scdata = '  *** Deleted ***'
      ***** END ADD ******* 04/29/08 **********************************
     C                   ENDSR


     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
      ***** BEG ADD ******* 04/29/08 **********************************
     c                   eval      @@rrn = scdbrr
      ***** END ADD ******* 04/29/08 **********************************
     C                   ENDSR


     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
B01  C     EACHK#        IFEQ      *ALL'9'
     C                   MOVE      *OFF          MEETSS
E01  C                   ENDIF
     C*** BEGIN ADD *** 03/03/05 **************************************
     C     Q1PRMS        IFGT      4
     C                   MOVEL     'N'           SCGRST
     C     MULTIG        IFEQ      *ON
     C*
     C                   CALL      'SA008R'
     C                   PARM      ##CO#         $$CO#             3 0
     C                   PARM      Q1USER        $$USR            10
     C                   PARM      'W'           $$APPL            1
     C     FAILED        PARM      *OFF          $$RTCD            1
     C                   PARM      ' '           $$AUTH            1
     C                   PARM      EAFND         $$FND             3 0
     C                   PARM      EAEMP#        $$EMP#            9 0
     C*
     C     FAILED        IFEQ      *ON
     C                   MOVEL     'Y'           SCGRST
     C                   ELSE
     C     $$AUTH        IFEQ      'I'
     C                   MOVEL     $$AUTH        SCGRST
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ELSE
     C     UPXFND        IFEQ      'Y'
     C     UPFND         ANDNE     EAFND
     C     UPXEMP        OREQ      'Y'
     C     UPFND         ANDNE     EAFND
     C     UPXEMP        OREQ      'Y'
     C     UPEMP#        ANDNE     EAEMP#
     C     UPXEMP        OREQ      'Y'
     C     UPDIV         ANDNE     EADIV
     C                   MOVEL     'Y'           SCGRST
     C                   ENDIF
     C                   ENDIF
     C*
     C     SSGRST        IFNE      *BLANKS
     C     SSGRST        ANDEQ     'N'
     C     SCGRST        ANDEQ     'Y'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 03/03/05 **************************************
      ***** BEG ADD ******* 04/29/08 **********************************

     c                   if        (ssfnd <> 0) and (eafnd <> ssfnd)
     c                   eval      meetss = *off
     c                   endif

     c                   if        (ssemp# <> 0) and (eaemp# <> ssemp#)
     c                   eval      meetss = *off
     c                   endif

     c                   if        (ssdiv <> 0) and (eadiv <> ssdiv)
     c                   eval      meetss = *off
     c                   endif

     c                   if        (ssfyr <> 0) and (eafyr <> ssfyr)
     c                   eval      meetss = *off
     c                   endif

     c                   if        (sschk# <> 0) and (eachk# <> sschk#)
     c                   eval      meetss = *off
     c                   endif

     C                   if        ssemnm <> *blanks
     C     emkey         chain     wmeml                              99
     C     LO:UP         xlate(p)  emname        u1pdsc
     C     LO:UP         xlate(p)  ssemnm        u2pdsc
     C                   exsr      string
     C                   endif
      *
      ***** END ADD ******* 04/29/08 **********************************
      ***** BEG ADD ******* 01/04/10 **********************************
     c                   if        (ssinv# <> 0) and (eainv# <> ssinv#)
     c                   eval      meetss = *off
     c                   endif
      ***** END ADD ******* 01/04/10 **********************************
     C                   ENDSR


     C*================================================================
     C* TOTCHK - Check totals for a difference
     C*================================================================
     C     TOTCHK        BEGSR
B01  C     SCCKAM        IFNE      SCCKTO
     C*** BEG ADD *** 04/18/05 ************************************    **
      *  If a PEO Group, ignore the edit
     C     XXPEOF        ANDNE     'M'
     C*** END ADD *** 04/18/05 ************************************    **
     C     SCCKAM        SUB       SCCKTO        SCCKDF
     C                   MOVE      SVCHK#        SCCHK#
     C                   MOVE      SVEMP#        SCEMP#
      ***** BEG ADD ***** 01/16/04 ************************************
     C                   MOVE      SVFND         SCGRP
      ***** END ADD ***** 01/16/04 ************************************
     C                   ADD       1             SFRRN4            4 044
     C                   WRITE     SCR04S
E01  C                   ENDIF
     C                   ENDSR


     C*================================================================
     C* TOTGET - Get totals for subfile display
     C*================================================================
     C     TOTGET        BEGSR
      ***** BEG ADD ***** 01/16/04 ************************************
     C     *LIKE         DEFINE    EAFND         SVFND
      ***** END ADD ***** 01/16/04 ************************************
     C     *LIKE         DEFINE    EAEMP#        SVEMP#
     C     *LIKE         DEFINE    EADIV         SVDIV
     C     *LIKE         DEFINE    EACHK#        SVCHK#
     C***  BEGIN ADD  ***  05/05/98  *********************************
     C     *LIKE         DEFINE    EADT          SVDT
     C     *LIKE         DEFINE    EATIME        SVTIME
     C***  END   ADD  ***  05/05/98  *********************************
     C***  BEGIN ADD  ***  01/31/00  *********************************
     C     *LIKE         DEFINE    EACKAM        SVCKAM
     C***  END   ADD  ***  01/31/00  *********************************
     C                   SUB       SCCKAM        SCCKAM
     C                   SUB       SCCKTO        SCCKTO
     C                   SUB       SCCKDF        SCCKDF
     C                   SUB       SVEMP#        SVEMP#
     C                   SUB       SVDIV         SVDIV
     C                   SUB       SVCHK#        SVCHK#
      ***** BEG ADD ***** 01/16/04 ************************************
     C                   CLEAR                   SVFND
      ***** END ADD ***** 01/16/04 ************************************
     C***  BEGIN ADD  ***  05/05/98  *********************************
     C                   CLEAR                   SVDT
     C                   CLEAR                   SVTIME
     C***  END   ADD  ***  05/05/98  *********************************
     C***  BEGIN ADD  ***  01/31/00  *********************************
     C                   CLEAR                   SVCKAM
     C***  END   ADD  ***  01/31/00  *********************************
     C*
     C*** BEG ADD *** 04/18/05 ************************************    **
     C*** BEG CHG *** 07/25/05 ************************************    **
     C********** 1         CHAINSSYPF                98
     C**********           MOVELSYFL12    XXFL12  1
     C********** XXFL12    IFEQ 'Y'
     C                   MOVEL     'WC'          SYKY              2
     C     SYKY          CHAIN     SWC01                              60
     C     WCFL30        IFEQ      'Y'
     C*** END CHG *** 07/25/05 ************************************    **
      *  Check WMFNP/FNPEOF flag for a PEO Group
     C                   MOVE      *BLANKS       XXPEOF            1
     C     FNKY          CHAIN     WMFNL                              98
B01  C     *IN98         IFEQ      *OFF
     C     FNPEOF        IFEQ      'M'
     C***  Begin Add  ***  08/22/07  **************************************
     C     FNPEOF        OREQ      'Y'
     C***  End   Add  ***  08/22/07  **************************************
     C*** BEGIN ADD *** 07/17/17 **************************************
     C     FNPEOF        OREQ      'C'
     C*** END ADD ***** 07/17/17 **************************************
     C                   MOVE      'M'           XXPEOF
     C                   ENDIF                                                  End FNPEOF = Y
E01  C                   ENDIF                                                  End *IN99 = off
     C                   ENDIF                                                              off
     C*** END ADD *** 04/18/05 ************************************    **
     C*
     C     BATKEY        CHAIN     WDEAL                              98
B01  C     *IN98         DOWEQ     *OFF
B02  C     EACKAM        IFNE      *ALL'9'
     C***  BEGIN ADD  ***  08/11/98  *********************************
B02  C     EACHK#        ANDNE     *ZEROS
     C***  END   ADD  ***  08/11/98  *********************************
     C***  BEGIN CHG  ***  05/05/98  *********************************
B03  C********** EAEMP#    IFNE SVEMP#
B03  C********** EADIV#    ORNE SVDIV
B03  C********** EACHK#    ORNE SVCHK#
     C     EACHK#        IFNE      SVCHK#
     C     EAemp#        OrNE      SVEMP#
     C***  END   CHG  ***  05/05/98  *********************************
     C***  BEGIN ADD  ***  01/31/00  *********************************
     C     EACKAM        ORNE      SVCKAM
     C***  END   ADD  ***  01/31/00  *********************************
     C                   EXSR      TOTCHK
     C                   MOVE      EAEMP#        SVEMP#
     C                   MOVE      EADIV         SVDIV
     C                   MOVE      EACHK#        SVCHK#
     C                   MOVE      EACKAM        SCCKAM
      ***** BEG ADD ***** 01/16/04 ************************************
     C                   MOVE      EAFND         SVFND
      ***** END ADD ***** 01/16/04 ************************************
     C***  BEG CHG ******  09/17/02 **********************************
     C*******************  MOVE *ZEROS    SCCKTO
     C                   Z-ADD     EAAMT         SCCKTO
     C***  END CHG ******  09/17/02 **********************************
     C***  BEGIN CHG  ***  03/08/00  *********************************
     C*****BEGIN ADD  ***  01/31/00  *********************************
     C**********           CLEARSVCKAM
     C*****END** ADD  ***  01/31/00  *********************************
     C                   MOVE      EACKAM        SVCKAM
     C***  END   CHG  ***  03/08/00  *********************************
      ***** BEG ADD ******* 01/13/03 **********************************
      *
     C                   ELSE
      *
     C                   ADD       EAAMT         SCCKTO
      ***** END ADD ******* 01/13/03 **********************************
E03  C                   ENDIF
      *
     C***  BEG DEL ******  09/17/02 **********************************
     C*******************  ADD  EAAMT     SCCKTO
     C***  END DEL ******  09/17/02 **********************************
E02  C                   ENDIF
     C     BATKEY        READE     WDEAL                                  98
E01  C                   ENDDO
     C                   EXSR      TOTCHK
     C                   ENDSR
      *** beg add ***** 04/29/08 ***********************************************
      *===============================================================
      * STRING - SUBROUTINE TO SEARCH STRINGS
      *===============================================================
     C     string        begsr

     C                   call      'QCLSCAN'
     C                   parm                    u1pdsc           40
     C                   parm      40            strlen            3 0
     C                   parm      1             start             3 0
     C                   parm                    u2pdsc           40
     C                   parm      40            patlen            3 0
     C                   parm      '0'           trans             1
     C                   parm      '1'           trim              1
     C                   parm      ' '           wild              1
     C                   parm                    scnres            3 0
      *
     C                   if        scnres = 0
     C                   eval      meetss = *off
     C                   endif
      *
     C                   endsr
      *** end add ***** 04/29/08 ***********************************************


** Column headings
Opt   number       amount         amount    Post date AR description
Opt Grp    number   Div  name
Opt       amount  Grp  Employer   Div  Yr name
Opt     number   number        amount        amount  Post date  AR description
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change View          YYY
212F12=Cancel               YYY
215F15=Fold/truncate        YYY
217F17=Subset               YYY
3
