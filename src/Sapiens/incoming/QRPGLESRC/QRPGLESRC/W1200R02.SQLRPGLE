     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define WEB
      *****************************************************************
     h/title ('A/R Batch Information')
      *****************************************************************
      *  Created:                                                     *
      *  01/16/98  C:69  Project: WC753                               *
      *****************************************************************
      * REVISIONS:                                                    *
      *                                                               *                            *
      * 01/07/25 C:342 Support: 118051                                *
      *                1.removed edit on the add of A/R Batch to allow*
      *                  Cash Dissolve on EFT                         *
      *                                                               *                            *
      * 11/06/24 C:342 Support: 117962                                *
      *                1.Delete record from Wxrip while exit.         *
      *                2.populate user in Wxrip file.                 *
      *                3.do not populate fnd# and emp# in Wxrip file. *
      *                4.do not lock Wxrip record in display mode.    *
      *                5.Delete record from Wxrip while exit for 8 and*
      *                  9 option from W1200R screen.                 *
      *                                                               *
      * 06/24/24 C:079 Project: 146WKR                                *
      *                Collateral Processing - Recomile               *
      *                                                               *
      * 04/04/24 C:062 Support: 115314(129)                           *
      *                Don't allow entry of transaction date if       *
      *                third party batch.                             *
      *                                                               *
      * 07/29/22 C:340 Support: 103003                                *
      *                PayGo IVC : Display A/R Batch Type Desc.       *
      *                                                               *
      * 05/24/21 C:180 SUPPORT 95396                                  *
      *                Prevent batch from being deleted while in use. *
      *                                                               *
      * 08/07/13 C:180 SUPPORT WA146 83111                            *
      *                Remove edit under 82528 which became           *
      *                unnecessary after 80364 was coded in this pgm. *
      *                Support 80364 was coded in this pgm on         *
      *                06/17/13 after 82528.                          *
      *                                                               *
      *                Support item 82528 is no longer needed - The   *
      *                fix was originally for client 146 only, but    *
      *                with 80364 the fix is now for all clients.     *
      *                                                               *
      * 06/17/13 C:180 Support: WA133 80364                           *
      *                Corrected date of changes in this pgm from     *
      *                07/17/12 to 06/17/13.                          *
      *                For record keeping: support 80364 was added    *
      *                to this pgm on 06/17/13 with a 07/17/12 date.  *
      *                This support item makes support item 82528     *
      *                no longer needed since the fix is not just     *
      *                for client 146, but for all clients.           *
      *                                                               *
      * 06/11/13 C:180 SUPPORT WA146 82528                            *
      *                Allow trans date change for SR-ONLINE ACH      *
      *                batches for Synergy.                           *
      *                                                               *
      * 07/17/12 C:136 Support: WA133 80364                           *
      *                Adjust edit on Cash Dissolve and EFT flag      *
      *                so that it invokes in add mode only. Also,     *
      *                protect EFT and Cash Dissolve flag in change   *
      *                mode - change in screen file.                  *
      *                                                               *
      * 09/06/11 C:277 Support 77890                                  *
      *                Change all ACH on screen to EFT.               *
      *                Fix already applied under Project 114WYI       *
      *                dated : 04/21/11                               *
      *                                                               *
      * 08/22/11 C:65  PROJECT: WC1851 P#164                          *
      *                Do not allow 'F' type batch on Add.            *
      *                                                               *
      * 05/20/11 C:190 PROJECT: WC1851 P#164                          *
      *                Remove the A/R batch type of "F-Fee".          *
      *                                                               *
      * 05/18/11 C:255 Support 76789 WA099                            *
      *                Change ACH security check to use SMCOP instead *
      *                of WMCBP.                                      *
      *                                                               *
      * 04/21/11 C:190 PROJECT: 114WYI                                *
      *                Change description on screen from              *
      *                "A=ACH" to "A=EFT"                             *
      *                                                               *
      * 02/15/11 C:061 PROJECT: WC1851                                *
      *              - When WCFL44 is set, allow 'F'ee.               *
      *                                                               *
      * 01/13/11 C:257 PROJECT: WC1948                                *
      *                If the A/R Batch has been marked 'Y' for       *
      *                approved and/or 'Y' for transfer display a     *
      *                pop-up window                                  *
      *                                                               *
      * 09/07/10 C:136 PROJECT: 71WKU PRISS                           *
      *                ALLOW MANUAL ENTRY OF RETRO BATCH              *
      *                                                               *
      * 08/30/10 C:61  Project sy190 priss 131 - WC1838
      *              - Change text in heading.  Program and screen    *
      *                changes.                                       *
      *              - Print "A/R Batch Type" choice descriptions     *
      *                and wrap text.                                 *
      *                                                               *
      * 08/09/10 C:191 Project: 71WKU                                 *
      *                Add Retro batch type.                          *
      *                                                               *
      * 03/25/10 C:33  Project: WC1838                                *
      *                Add deductible batch type.                     *
      *                                                               *
      * 09/23/08 C:136 Support: WA146 64767                           *
      *                Ensure transaction date is not changed to      *
      *                a value in which an agent history record       *
      *                doesn't exist for a batch detail record        *
      *                                                               *
      * 08/07/06 C:136  Support: WA109 54508                          *
      *                 Prevent EACASH value from being changed to    *
      *                 non-dissolved status after being dissolved.   *
      *                 This re-implements 52703 which was lost in    *
      *                 the conversion to ALDON.                      *
      *                                                               *
      * 06/20/06 C:136  Project: WC1324                               *
      *                 Add new batch type of 'S'                     *
      *                                                               *
      * 03/13/06 C:136  Support: WA136 50196                          *
      *                 Prevent transaction date from being in the    *
      *                 past by more than one year.                   *
      *                                                               *
      * 11/01/05  C:33  Project: 109WDZ  WA109/49559                  *
      *                 {109AN}                                       *
      *                 For CLT109, allow for entry of transaction    *
      *                  date                                         *
      *                                                               *
      * 10/07/05 C:136  Project: WC1179                               *
      *                 Enable SCCASH with values of (Y/ )            *
      *                                                               *
      * 10/06/05  C:33  Project: 109WDK                               *
      *                 Temporarily remove SCCASH {DSPATR(ND) used in *
      *                  DDS}                                         *
      *                                                               *
      * 04/07/05       PROJECT: 126WDK                                *
      *          C:65  PROJECT: 126WAL                                *
      *                ADD SUBROUNTINE FOR MASS UPDATE OF CHECK# AND  *
      *                POSTING DATE.                                  *
      *                                                               *
      * 02/22/05 C:62 PROJECT 129WJO                                  *
      *               -ADD ACH SECURITY                               *
      *         NOTE: - This currently is in c129.pgmrd until WC1179  *
      *                 is implemented, 129WJO changes have been      *
      *                 added to this code.  (C:132 note)             *
      *                                                               *
      * 09/07/04 C:132 Support: WC1179 Auto Cash Dissolve             *
      *                Add Cash Dissolve Flag on Screed and edit it   *
      *                for a (Y/N). If is a "Y", all and API program  *
      *                to auto dissolve the batch payments.           *
      *                                                               *
      *                The API call will be in MVSCBD subroutine.     *
      *                                                               *
      * 06/16/04 C:136 Support: WA033 42902                           *
      *                Transaction dates were zeroed out. Solution:   *
      *                Prevent user from changing EAACHF (Lockbox/    *
      *                ACH flag) after detail records have been       *
      *                added. User will have to delete and reenter    *
      *                batch if this flag needs to be changed.        *
      *                                                               *
      * 09/24/03 C:136 Project: WC1116                                *
      *                Add processing for initial maintenance fields. *
      *                The initial date field will be used as the     *
      *                ACH batch creation date.                       *
      *                                                               *
      * 04/16/03 C:142 WC1117  LOCK BOX PROCESSING CHANGES            *
      *                                                               *
      * 04/11/03 C:136 PROJECT: WC1116                                *
      *                Add code if ACH processing is enabled.         *
      *                                                               *
      * 06/03/02 C:136 SUPPORT: WA121 SUPPORT: 32817                  *
      *                CORRECTED FIELD DEFINITION IN DISPLAY FILE     *
      *                FROM 11.0 TO 11.2 TO ACCOMODATE USE OF DECIMAL *
      *                NUMBERS.                                       *
      *                                                               *
      * 04/19/02 C:136 PROJECT: WC577                                 *
      *                CHANGE SCREEN FILE TO ACCOMODATE THE BATCH AMT *
      *                FIELD BEING CHANGED FROM A 9.2 TO AN 11.2      *
      *****************************************************************
     FW1200F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      *
     FWDEAL4    UF   E           K DISK
      *  Physical file definition (for maintaining record)
     FWDEAP     UF A E             DISK
     F                                     RENAME(PEA01:DBRCDF)
     F                                     INFDS(DBFDS)
      ***** Begin Add  **** 04/16/03 ***************************************
     FSWCPF     IF   E             DISK
      ***** End   Add  **** 04/16/03 ***************************************
     F*** Begin Add *** 02/22/05 *******************************************
     F*** Beg   Del *** 05/18/11 *******************************************
     F*******wmcbl     IF   E           k DISK
     F*** End   Del *** 05/18/11 *******************************************
     F*** End   Add *** 02/22/05 *******************************************
     F*** Beg   Add *** 05/18/11 *******************************************
     Fsmcol     IF   E           k DISK
     F*** End   Add *** 05/18/11 *******************************************
      //*** Begin Add *** 05/24/21 *****************************************
      //*** Begin Chg *** 11/06/24 *****************************************
      //***  Dcl-F WXRIL3 disk usage(*update:*output) keyed;
       Dcl-F WXRIL3 disk usage(*update:*output:*delete) keyed;
      //*** End   Chg *** 11/06/24 *****************************************
      //**  End   Add *** 05/24/21 *****************************************
      *
     D DBFDS           DS
     D  DBRRN                397    400B 0
      *
      *================================================================
      *  /COPY required constants
      *================================================================
     D OPTNM           C                   CONST('A/R Batches')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)
     D***  Begin Add  ***  11/01/05  **************************************
      ******* beg del ****** 08/30/10 *********************************
     D*headAL          c                   '  If ''A'' or ''L'', enter -
     D*****                                trans date:'
     D*headL           c                   '  If ''L'', enter transaction -
     D*****                                date :'
     D*head109         c                   'Transaction date . . . . . . . . :'
      ******* end del ****** 08/30/10 *********************************
      ******* beg add ****** 08/30/10 *********************************
      ******* beg chg ****** 04/21/11 *********************************
     D*headAL*****     c                   '  If ACH or LOCKBOX, enter -
     D************                         trans date:'
     D headAL          c                   '  If EFT or LOCKBOX, enter -
     D                                     trans date:'
      ******* end chg ****** 04/21/11 *********************************
     D headL           c                   '  If LOCKBOX, enter transaction -
     D                                     date :'
     D head109         c                   '  Transaction date . . . . . . .-
     D                                      . . :'
      ******* end add ****** 08/30/10 *********************************
     D***  End   Add  ***  11/01/05  **************************************
     D Datusa          S               D   Datfmt(*Usa)
      ***** Begin Add  **** 04/16/03 ***************************************
     D Trdt1           S               D   Datfmt(*Usa)
     D Trdt2           S               D   Datfmt(*Usa)
      ***** End   Add  **** 04/16/03 ***************************************
      ***  beg add ***** 10/07/05 ******************************************
     d svcash          s                   like(EACASH)
      ***  end add ***** 10/07/05 ******************************************
     D***  Begin Add  ***  11/01/05  **************************************
     D cltID           s              3
     D***  End   Add  ***  11/01/05  **************************************
     D***  Begin Add  ***  11/01/05  **************************************
     D yrDiff          s              5  0
     D***  End   Add  ***  11/01/05  **************************************
     D***  Begin Add  ***  03/25/10  **************************************
     D useDed          s              1n
     D useLargeDed     s              1n
     D dedBatch        s              1n
     D***  End   Add  ***  03/25/10  **************************************
      ******* beg add ****** 08/30/10 *********************************
     D type_desc1      s            256a
     D type_desc2      s            256a
      ******* end add ****** 08/30/10 *********************************
      ******* beg add ****** 06/11/13 *********************************
     D xxonli          s                   like(EAONLI)
      ******* end add ****** 06/11/13 *********************************
      //*** Begin Add *** 05/24/21 ************************************
       //Keys Data Structures
       Dcl-Ds KeyWXRIL3 likerec(PRI01:*KEY);
      //*** End   Add *** 05/24/21 ************************************
      *
     C/COPY SCOPYMT01C
      *================================================================
      *  PLIST definitions
      *================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C***  begin chg  ***  04/07/05 ***********************************
     C                   PARM                    @@NCHK            7 0
     C                   PARM                    @@PODT            8 0
     C***  end   chg  ***  04/07/05 ***********************************
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*** BEGIN ADD *** 02/22/05 **************************************
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C*** END   ADD *** 02/22/05 **************************************
      ******* beg add ****** 08/30/10 *********************************
      *
      *  LINE FORMAT
      *
     C     LINEPL        PLIST
     C                   PARM                    ##LINE          256
     C                   PARM                    ##LEN             3 0
     C                   PARM                    ##REM           256
      ******* end add ****** 08/30/10 *********************************


      *================================================================
      *  KLIST definitions
      *================================================================
     C     EAKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    EABAT#
     C***  Begin Add  ***  01/13/11  **************************************
     C     eaky2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCBAT#
     C                   KFLD                    EAFND
     C                   KFLD                    EAEMP#
     C***  End   Add  ***  01/13/11  **************************************
     C*** Begin Add *** 02/22/05 *******************************************
     C*** Beg   Chg *** 05/18/11 *******************************************
     C*****cbky          KLIST
     C*****              KFLD                    ##CO#
     C     coky          KLIST
     C                   KFLD                    ##CO#
     C*** End   Chg *** 05/18/11 *******************************************
     C*** End   Add *** 02/22/05 *******************************************
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
      ***** Begin Add  **** 04/16/03 ***************************************
     C     *LIKE         Define    EATRDT        SVTRDT
      ***** End   Add  **** 04/16/03 ***************************************
     C***  Begin Add  ***  01/13/11  **************************************
     C     *LIKE         Define    scbata        ##bata
     C     *LIKE         Define    sccash        ##cash
     C     *LIKE         Define    scachf        ##achf
     C     *LIKE         Define    sctrdt        ##trdt
     C***  End   Add  ***  01/13/11  **************************************
     C*** BEGIN ADD *** 02/22/05 **************************************
     C*================================================================
     C* CHKSEC - Checks to see if there is sufficient security
     C*================================================================
     C     chksec        begsr
     C*
     C                   move      *OFF          nogo              1
     C*
     C*** Beg   Chg *** 05/18/11 *******************************************
     C*****cbky          chain     pcb01
     C     coky          chain     pco01
     C*** End   Chg *** 05/18/11 *******************************************
     C                   if        %found
     C*** Beg   Chg *** 05/18/11 *******************************************
     C*******            if        cbachl <> 0 or cbacht <> *blanks
     C                   if        coachl <> 0 or coacht <> *blanks
     C*** End   Chg *** 05/18/11 *******************************************
     C*
     C                   move      ##lvl         ##ulvl
     C*** Beg   Chg *** 05/18/11 *******************************************
     C*******            move      cbachl        ##clvl
     C                   move      coachl        ##clvl
     C*** End   Chg *** 05/18/11 *******************************************
     C*
     C                   call      'SRCCSC'
     C                   parm      ##type        ##utyp            1
     C                   parm                    ##ulvl            3
     C*** Beg   Chg *** 05/18/11 *******************************************
     C**********         parm      cbacht        ##ctyp            1
     C                   parm      coacht        ##ctyp            1
     C*** End   Chg *** 05/18/11 *******************************************
     C                   parm                    ##clvl            3
     C                   parm      '1'           ##exec            1
     C                   parm      ' '           ##res             1
     C*
     C                   if        ##res = *on
     C                   move      *on           nogo
     C                   endif
     C                   endif
     C                   endif
     C*
     C                   endsr
     C*** END   ADD *** 02/22/05 **************************************
      *================================================================
      * DLTPRE - Pre-processing before delete occurs
      *================================================================
     C     DLTPRE        BEGSR
     C     *IN99         DOUEQ     *ON
     C     EAKY1         DELETE    WDEAL4                             99
     C                   ENDDO
     C                   ENDSR
      *================================================================
      * DSPLYR - Processing after displaying information
      *================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
      *================================================================
      * ERRCK1 - Error checking for srceen 1 (SCR01)
      *================================================================
     C     ERRCK1        BEGSR

     C***  Begin Add  ***  03/25/10  **************************************
     C                   if        *in09
     C                   leavesr
     C                   endif
     C***  End   Add  ***  03/25/10  **************************************
     C***  Begin Add  ***  01/13/11  **************************************
     C                   if        scbata <> ##bata
     C                             or sccash <> ##cash
     C                             or scachf <> ##achf
     C                             or sctrdt <> ##trdt
     C     eaky2         chain(n)  pea01                              60
     c                   if        eatffl = 'Y' or eatfap = 'Y'
     c                   exfmt     wdw01
     C                   seton                                          90
     c                   endif                                                  eatffl
     c                   endif                                                  scbata
     C***  End   Add  ***  01/13/11  **************************************
      *
      ******* beg add ****** 02/15/11 *********************************
     c                   if        (wcfl44 = 'Y') and (scACHF = 'F')
     c*** begin add ***  08/22/11  *****************************************
     c                   if        *in40
     c                   seton                                        71  90
     c                   endif
     c*** end add *****  08/22/11  *****************************************
     c                   else
      ******* end add ****** 02/15/11 *********************************
      *** beg add ***** 04/11/03 *******************************************
     C                   if        *In55
      *
     C***  Begin Chg  ***  03/25/10  **************************************
     C**********         if        sclkb1 <> 'A' and
     C**********                   sclkb1 <> 'L' and
     C**********                   sclkb1 <> ' '
      ****beg*add ***** 06/20/06 *******************************************
     c**********                   and sclkb1 <> 'S'
      ****end*add ***** 06/20/06 *******************************************
     C  n90              if        scAchF <> 'A'
     C                              and scAchF <> 'L'
     c                              and scAchF <> 'S'
     C                              and scAchF <> ' '
     C                              and not dedBatch
     C*** BEGIN ADD ****** 08/09/10 ************************************
     C*** beg chg ***** 09/07/10 ***************************************
     C***********                   and (scachf = 'R' and (cltid <> '071'
     C***********                   or @@mode <> 'C'))
     C                              and (scachf = 'R' and (cltid <> '071'))
     C*** end chg ***** 09/07/10 ***************************************
     C*** END   ADD ****** 08/09/10 ************************************
     C***  End   Chg  ***  03/25/10  **************************************
     c                   seton                                        71  90
     C                   endif
      *
      *** beg add ***** 06/20/06 *******************************************
     C***  Begin Chg  ***  03/25/10  **************************************
     c**********         if        sclkb1 = 'S' and
     c                   if        scAchF = 'S' and
     C***  End   Chg  ***  03/25/10  **************************************
     c                             (@@mode = 'C' or *in40)
     c                   seton                                        85  90
     c                   endif
      *** end add ***** 06/20/06 *******************************************
     C*** Begin add *** 02/22/05 **************************************
     C*
     C* Do not allow an ACH batch if the user does not have the security
     C*
     C***  Begin Chg  ***  03/25/10  **************************************
     C**********         if        not *in90 and sclkb1 = 'A' and
     C**********                   nogo = *on
     C  n90              if        scAchF = 'A'
     C                              and nogo = *on
     C***  End   Chg  ***  03/25/10  **************************************
     C***  Beg   Chg  ***  05/18/11  **************************************
     C********           seton                                        8990
     C                   seton                                        9190
     C***  End   Chg  ***  05/18/11  **************************************
     C                   endif
     C*** End   add *** 02/22/05 **************************************
      *** beg add ***** 06/16/04 *******************************************
      *
      * Detail records will have a group number. The initial record that has
      * group number equal to zero has been deleted by W1210R in PGMEND.
      *
     C***  Begin Chg  ***  03/25/10  **************************************
     c**n90*****         if        EAACHF <> sclkb1 and
     c  n90              if        EAACHF <> scAchF and
     C***  End   Chg  ***  03/25/10  **************************************
     c                             EAFND  <> 0
     c                   seton                                        79  90
     c                   endif
      *** end add ***** 06/16/04 *******************************************
      *
     C***  Begin Add  ***  11/01/05  **************************************
     C                   if        cltID <> '109'
     C***  End   Add  ***  11/01/05  **************************************
     C***  Begin Chg  ***  03/25/10  **************************************
     c**n90*****         if            sclkb1 = *blank
     c**********                   and sctrdt1 <> *zero
     c  n90              if            scAchF = *blank
     c                             and scTrDt <> *zero
     c*** Begin Add *** 04/04/24 ******************************************
     c                             or scachf = 'T'
     c                             and sctrdt <> *zero
     c                             and cltID = '129'
     c*** End   Add *** 04/04/24 ******************************************
     c*** BEGIN ADD ****** 08/09/10 ************************************
     c                             or scachf = 'R'
     c                             and sctrdt <> *zero
     c                             and @@mode = 'A'
     c*** END   ADD ****** 08/09/10 ************************************
     C***  End   Chg  ***  03/25/10  **************************************
     c                   seton                                        72  90
     c                   endif
     C***  Begin Add  ***  11/01/05  **************************************
     c                   endif
     C***  End   Add  ***  11/01/05  **************************************
      *
     C***  Begin Chg  ***  03/25/10  **************************************
     C***  Begin Chg  ***  03/25/10  **************************************
     c**n90*****         if            sclkb1  = 'A'
     c**********                   and sctrdt1 = 0  or
     c**********                       sclkb1  = 'L'
     c**********                   and sctrdt1 = 0
     c  n90              if        (scAchF = 'A' or scAchF = 'L')
     c                              and scTrDt = *zero
     C***  End   Chg  ***  03/25/10  **************************************
     c                   seton                                        73  90
     c                   endif                                                           srerr

     C***  Begin Chg  ***  03/25/10  **************************************
     c**n90*****         if        sctrdt1 <> 0
     c**********         eval      srdti = sctrdt1
     c**********         call      'SRCVTDAT'    datchk
     c**********         movel     srdto         sctrdt1
     c  n90              if        scTrDt <> 0
     c                   eval      srdti = scTrDt
     c                   call      'SRCVTDAT'    datchk
     c                   movel     srdto         scTrDt
     C***  End   Chg  ***  03/25/10  **************************************
      *
     c                   if        srerr = '1'
     c                   seton                                        74  90
     c                   endif                                                           srerr
      *
      *** beg add ***** 03/13/06 ***********************************************
     c     *in90         ifeq      *off
     c                   time                    dsdttm
     C***  Begin Chg  ***  03/25/10  **************************************
     c******usa*         move      sctrdt1       trdt1
     c     *usa          move      scTrDt        trdt1
     C***  End   Chg  ***  03/25/10  **************************************
     c     *usa          move      dsdate        datUsa
     c     datusa        subdur    trdt1         yrDiff:*y
     c                   if        yrDiff >= 1
     c                   seton                                        83  90
     c                   endif
     c                   endif
      *** end add ***** 03/13/06 ***********************************************
      *
     c                   endif                                                           sctrdt1<>0
      *
     c                   endif                                                           *in55
      *
     C                   if        not *in55
      *
     C***  Begin Chg  ***  03/25/10  **************************************
     C**********         if        sclkb2 <> 'L' and
     C**********                   sclkb2 <> ' '
     C                   if        scAchF <> 'L'
     C                              and scAchF <> ' '
     C***  End   Chg  ***  03/25/10  **************************************
     c                   seton                                        75  90
     C                   Endif
      *** beg add ***** 06/16/04 *******************************************
      *
      * Detail records will have a group number. The initial record that has
      * group number equal to zero has been deleted by W1210R in PGMEND.
      *
     C***  Begin Chg  ***  03/25/10  **************************************
     c**n90*****         if        EAACHF <> sclkb2 and
     c  n90              if        EAACHF <> scAchF and
     C***  End   Chg  ***  03/25/10  **************************************
     c                             EAFND  <> 0
     c                   seton                                        79  90
     c                   endif
      *** end add ***** 06/16/04 *******************************************
      *
     C***  Begin Add  ***  11/01/05  **************************************
     C                   if        cltID <> '109'
     C***  End   Add  ***  11/01/05  **************************************
     C***  Begin Chg  ***  03/25/10  **************************************
     c**n90*****         if            sclkb2  = *blank
     c**********                   and sctrdt2 <> *zero
     C***  Begin Chg  ***  04/04/24  **************************************
     c**n90******        if        scAchF = *blank
     c***********                   and scTrDt <> *zero
     c                   If        (*in90 = *Off) And
     c                             (scAchF = *blank Or
     C                              (scAchF = 'T' And cltID = '129'))
     c                              and (scTrDt <> *zero)
     C***  End   Chg  ***  04/04/24  **************************************
     C***  End   Chg  ***  03/25/10  **************************************
     c                   seton                                        76  90
     c                   endif
     C***  Begin Add  ***  11/01/05  **************************************
     c                   endif
     C***  End   Add  ***  11/01/05  **************************************
      *
     C***  Begin Chg  ***  03/25/10  **************************************
     c**n90*****         if            sclkb2  = 'L'
     c**********                   and sctrdt2 = 0
     c  n90              if        scAchF = 'L'
     c                              and scTrDt = 0
     C***  End   Chg  ***  03/25/10  **************************************
     c                   seton                                        77  90
     c                   endif
      *
     C***  Begin Chg  ***  03/25/10  **************************************
     c**n90*****         if        sctrdt2 <> 0
     c**********         eval      srdti = sctrdt2
     c**********         call      'SRCVTDAT'    datchk
     c**********         movel     srdto         sctrdt2
     c  n90              if        scTrDt <> 0
     c                   eval      srdti = scTrDt
     c                   call      'SRCVTDAT'    datchk
     c                   movel     srdto         scTrDt
     C***  End   Chg  ***  03/25/10  **************************************
      *
     c                   if        srerr = '1'
     c                   seton                                        78  90
     c                   endif                                                  srerr
      *
      *** beg add ***** 03/13/06 ***********************************************
     c     *in90         ifeq      *off
     c                   time                    dsdttm
     C***  Begin Chg  ***  03/25/10  **************************************
     c******usa*         move      sctrdt2       trdt2
     c     *usa          move      scTrDt        trdt2
     C***  End   Chg  ***  03/25/10  **************************************
     c     *usa          move      dsdate        datUsa
     c     datusa        subdur    trdt2         yrDiff:*y
     c                   if        yrDiff >= 1
     c                   seton                                        84  90
     c                   endif
     c                   endif
      *** end add ***** 03/13/06 ***********************************************
     c                   endif                                                  not *in90
      *
     C                   endif                                                  not *in55
      *** end add ***** 04/11/03 *******************************************
      *** beg add ***** 09/23/08 *******************************************
     c  n90              if        @@mode = 'C'

     C***  Begin Chg  ***  03/25/10  **************************************
     c**********         select
     c**********         when      sctrdt1 <> 0
     c******cymd         move      trdt1         transDate         7 0
     c**********         when      sctrdt2 <> 0
     c******cymd         move      trdt2         transDate
     c**********         endsl
     c                   eval      srdti = scTrDt
     c                   call      'SRCVTDAT'    dtCYMD
     c                   movel     srdto         transDate         7 0
     C***  End   Chg  ***  03/25/10  **************************************

     c                   if        transDate <> 0
     c                   z-add     0             xxcnt             5 0
     c/exec SQL
     c+  select  count(*) into :xxcnt from wdeap a
     c+    where a.eabat# = :scbat# and
     c+          a.eaemp# <> 0 and
     c+          not exists(select * from wmahp b
     c+                       where  b.ahco#  = a.eaco#  and
     c+                              b.ahfnd  = a.eafnd  and
     c+                              b.ahemp# = a.eaemp# and
     c+                              b.ahdiv  = a.eadiv  and
     c+                              b.ahfyr  = a.eafyr  and
     c+                              b.ahefdt <= :transDate and
     c+                              b.ahstat = 'A' )
     c/end-exec

     c                   if        xxcnt > 0
     c                   seton                                        89  90
     c                   endif
     c                   endif

     c                   endif
      *** end add ***** 09/23/08 *******************************************
      *
      ***** BEG ADD ***** 09/07/04 *************************************
      * Edit auto cash dissolve flag
      *** beg chg ***** 10/07/05 ***********************************************
     C*********          if        sccash <> 'Y' AND sccash <> 'N'
     C  n90              if        sccash <> 'Y' AND sccash <> ' '
      *** beg chg ***** 10/07/05 ***********************************************
     C                   seton                                        80  90
     C                   endif
      *** beg del ***** 10/07/05 ***********************************************
     C**********         IF        sccash = 'Y'
     C**********         move      *on           *in39
     C**********         Else
     C**********         move      *OFF          *in39
     C**********         endif
      *** end del ***** 10/07/05 ***********************************************
      ***** END ADD ***** 09/07/04 *************************************
      *** beg add ***** 10/07/05 ***********************************************
     c  n90              if        *in40 = *off and
     c                             sccash <> svcash
     c                   seton                                        81  90
     c                   endif

     C***  Begin Chg  ***  03/25/10  **************************************
     c**n90*****         if        sccash = 'Y' and
     c**********                   sclkb1 = 'A'
     C*** beg chg ***** 07/17/12 ***********  06/17/13 ********************
     c**n90******        if        scCash = 'Y'
     c***********                   and scAchF = 'A'
      *
      * Invoke this edit in add mode only. You can't make an ACH batch a
      * cash dissolve batch.
      *
     c******* Beg Del ***** 01/07/25 ******************************************
     c**n90              if        scCash = 'Y'
     c***                           and scAchF = 'A'
     c***                           and *in40 = *on
     C*** end chg ***** 07/17/12 ************* 06/17/13 *******************
     C***  End   Chg  ***  03/25/10  **************************************
     c***                seton                                        82  90
      ******* BEG remove *****  08/07/13 ******************************
      ******* Beg add ****** 06/11/13 *********************************
       // ***/Free
       // *** If cltid = '146' and xxonli = 'Y';
       // ***     *in82 = *OFF;
       // ***     *in90 = *OFF;
       // *** Endif;
       // ****/End-Free
      ******* End add ****** 06/11/13 *********************************
      ******* end remove *****  08/07/13 ******************************
     c***                endif
     c******* End Del ***** 01/07/25 ******************************************
      *** end add ***** 10/07/05 ***********************************************
      ******* beg add ****** 02/15/11 *********************************

     c                   endif
      ******* end add ****** 02/15/11 *********************************
      *
     C                   Endsr
      *================================================================
      * ERRCK2 - Error checking for srceen 2 (SCR02)
      *================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
      *================================================================
      * ERRCK3 - Error checking for srceen 3 (SCR03)
      *================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C***  begin add  ***  04/07/05 ***********************************
     C*================================================================
     C* MASSUP - UPDATE CHECK# AND POSTING DATE OF BATCH
     C*================================================================
     C     MASSUP        BEGSR
     C*
     C     EAKY1         CHAIN     WDEAL4                             99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     @@NCHK        EACHK#
     C                   Z-ADD     @@PODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C                   EXSR      ZUPFLD
     C                   UPDATE    PEA01
     C     EAKY1         READE     WDEAL4                                 99
     C                   ENDDO
     C*
     C                   ENDSR
     C***  end   add  ***  04/07/05 ***********************************
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
     C                   MOVE      EABATA        SCBATA
      *** Beg add ***** 06/11/13 ***********************************************
      /FREE
        xxonli = *blanks;
      /END-FREE
      *** End add ***** 06/11/13 ***********************************************
      *** beg add ***** 06/20/06 ***************************************
      *
      * No updating allowed if batch is a payroll or premium upload type
      *
     c                   if        eaachf = 'S'
     c***  Begin Add  ***  03/25/10  **************************************
     C                              or EAACHF = 'D'
     C
     c***  End   Add  ***  03/25/10  **************************************
     c                   seton                                        09
     c                   endif
      *** end add ***** 06/20/06 ***************************************
      *
      ***** BEG ADD ***** 09/07/04 *************************************
     C                   MOVE      EACASH        SCCASH
      ***** END ADD ***** 09/07/04 *************************************
      ***  Begin Add  ***  10/06/05  **************************************
      ***  beg del ***** 10/07/05 *****************************************
     C***************    eval      sccash = 'N'
      ***  edg del ***** 10/07/05 *****************************************
      *** beg add ***** 10/07/05 ***************************************
     c                   if        EACASH <> 'D'
     c                   eval      svcash = EACASH
     c                   else
     c                   eval      svcash = 'Y'
     c                   eval      sccash = 'Y'
     c                   endif
      *** end add ***** 10/07/05 ***************************************
      ***  End   Add  ***  10/06/05  **************************************

     C***  Begin Add  ***  03/25/10  **************************************
     C                   select
     C                   when      EAACHF = 'D'
      ******* beg chg ****** 08/30/10 *********************************
     C*******            eval      scBchTypD1 = %trim(scBchTypD1)
     C*******                                 + ' D=Deductible,'
     C                   eval      type_desc1 = %trim(type_desc1)
     C                                        + ' D=Deductible,'
      ******* end chg ****** 08/30/10 *********************************
     C                   eval      dedBatch = *on
     c                   other
     C                   eval      dedBatch = *off
     C                   endsl
     C***  End   Add  ***  03/25/10  **************************************
      *
      *** beg add ***** 04/11/03 *******************************************
     C***  Begin Chg  ***  03/25/10  **************************************
     C**********         if        *in55
     C**********         move      EAACHF        sclkb1
     c**********         eval      srdti = EATRDT
     c**********         call      'SRCVTDAT'    dtmdyy
     c**********         movel     srdto         sctrdt1
      **********
     C**********         else
     C**********         move      EAACHF        sclkb2
     c**********         eval      srdti = EATRDT
     c**********         call      'SRCVTDAT'    dtmdyy
     c**********         movel     srdto         sctrdt2
      **********
     c**********         endif
     C                   eval      scAchF = EAACHF
     c                   eval      srdti = EATRDT
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         scTrDt
     C***  End   Chg  ***  03/25/10  **************************************
      *
     c                   if        EAACHF = 'A'
     c                   seton                                        56
     c                   eval      scuser = EAUSER
     c                   eval      sctime = EATIME
      *** beg chg ***** 09/24/03 ***********************************************
     c*****************  eval      srdti  = EADT
     c                   eval      srdti  = EAIDT
      *** end chg ***** 09/24/03 ***********************************************
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         scdt
     c                   eval      scachu = EAACHU
     c                   eval      scacht = EAACHT
     c                   eval      srdti  = EAACHD
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         scachd
      *** Beg add ***** 06/11/13 ***********************************************
      /FREE
        xxonli = EAONLI;
      /END-FREE
      *** End add ***** 06/11/13 ***********************************************
     c                   else
     c                   setoff                                       56
     c                   endif
      *** end add ***** 04/11/03 *******************************************
      ******* beg add ****** 08/30/10 *********************************

      * assemble "Batch Type Description" text

     c                   eval      ##line = %trim(type_desc1) + ' ' +
     c                                      %trim(type_desc2)
     c                   eval      ##len = 34
     C                   CALL      'SRLINE'      LINEPL
     c                   eval      scbchtypd1 = ##line

     c                   eval      ##line = ##rem
     C                   CALL      'SRLINE'      LINEPL
     c                   eval      scbchtypd2 = ##line

     c                   eval      ##line = ##rem
     C                   CALL      'SRLINE'      LINEPL
     c                   eval      scbchtypd3 = ##line
      ******* beg add ****** 02/15/11 *********************************

     c                   eval      ##line = ##rem
     C                   CALL      'SRLINE'      LINEPL
     c                   eval      scbchtypd4 = ##line
      ******* end add ****** 02/15/11 *********************************

      ******* end add ****** 08/30/10 *********************************
     C***  Begin Add  ***  01/13/11  **************************************
     C                   move      scbata        ##bata
     C                   eval      ##cash = sccash
     C                   eval      ##achf = scachf
     c                   eval      ##TrDt = scTrDt
     C***  End   Add  ***  01/13/11  **************************************
      *** Beg add **** 08/07/06 ****************************************
         If *In40 = *Off;
            scbchtypd1 = '';
            scbchtypd2 = '';
            scbchtypd3 = '';
            scbchtypd4 = '';

            Type1_Pos = 568;

            Select;
            When SCACHF = 'A';
              scbchtypd1 = 'EFT';
            When SCACHF = 'L';
              scbchtypd1 = 'Lockbox';
            When SCACHF = 'S';
              scbchtypd1 = 'S/R Upload';
            //*** Begin Add *** 04/04/24 ***********************************
            When SCACHF = 'T';
              scbchtypd1 = 'Third Party';
            //*** End   Add *** 04/04/24 ***********************************
            When SCACHF = ' ';
              scbchtypd1 = 'Regular';
            EndSL;
         Else;
            Type1_Pos = 589;
         EndIf;
      *** End add **** 08/07/06 ****************************************
      *
     C                   ENDSR
      *================================================================
      * MVSCDB - Move screen fields to data base fields
      *================================================================
     C     MVSCDB        BEGSR
      *
      *** beg add **** 08/07/06 ****************************************
     c                   if        eacash <> 'D'
      *** end add **** 08/07/06 ****************************************
      ***** BEG ADD ***** 09/07/04 *************************************
     C                   EVAL      EACASH = SCCASH
      ***** END ADD ***** 09/07/04 *************************************
      *** beg add **** 08/07/06 ****************************************
     c                   endif
      *** end add **** 08/07/06 ****************************************
     C                   MOVE      SCBATA        EABATA
      *** beg add ***** 04/11/03 *******************************************
     C***  Begin Chg  ***  03/25/10  **************************************
     C**********         if        *in55
     C**********         move      sclkb1        EAACHF
     c**********         eval      srdti = sctrdt1
     c**********         call      'SRCVTDAT'    dtcymd
     c**********         movel     srdto         EATRDT
      **********
     C**********         else
     C**********         move      sclkb2        EAACHF
     c**********         eval      srdti = sctrdt2
     c**********         call      'SRCVTDAT'    dtcymd
     c**********         movel     srdto         EATRDT
      **********
     C**********         endif
     C                   eval      EAACHF = scAchF
     c                   eval      srdti = scTrDt
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         EATRDT
     C***  End   Chg  ***  03/25/10  **************************************
      *** end add ***** 04/11/03 *******************************************
      *
     C                   ENDSR
      *================================================================
      * OTHERR - Processing for other modes
      *================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
      *================================================================
      * OTHFKS - Processing for other functions keys
      *================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR
         //*** Begin Add *** 05/24/21 ************************************
             //*** Beg Chg *** 11/06/24 ************************************
             //*** Unlock WXRIL3;
             Monitor;
              delete pri01;
             On-Error;
             Endmon;
             //*** End Chg *** 11/06/24 ************************************
         //*** Endif Add *** 05/24/21 ************************************
      ******* beg add ****** 08/30/10 *********************************
     c                   eval      *inlr = *on
      ******* end add ****** 08/30/10 *********************************
     C                   ENDSR
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
      ******* beg add ****** 08/30/10 *********************************
     c                   eval      type_desc1 = *blanks
     c                   eval      type_desc2 = *blanks
      ******* end add ****** 08/30/10 *********************************
      *** Beg add ***** 06/11/13 ***********************************************
      /FREE
        xxonli = *blanks;
      /END-FREE
      *** End add ***** 06/11/13 ***********************************************
      *
     C***  Begin Add  ***  11/01/05  **************************************
     C                   call      'RTVCLTID'
     C                   parm                    cltID
     C***  End   Add  ***  11/01/05  **************************************

     C***  Begin Add  ***  03/25/10  **************************************
     C                   clear                   WCFL06
     C                   clear                   WCFL24
     C                   clear                   WCFL43
     C***  End   Add  ***  03/25/10  **************************************
      ***** Begin Add  **** 04/16/03 ***************************************
      ******* beg add ****** 02/15/11 *********************************
     c                   clear                   swc01
      ******* end add ****** 02/15/11 *********************************
     C                   READ      SWCPF                                  60
     C     *IN60         IFEQ      *OFF
     C     WCFL24        IFEQ      'Y'
     C                   MOVE      *ON           *IN55
     C                   ELSE
     C                   MOVE      *OFF          *IN55
     C                   ENDIF
     C                   ENDIF
      ***** End   Add  **** 04/16/03 ***************************************

     C***  Begin Add  ***  03/25/10  **************************************
      ******* beg chg ****** 08/30/10 *********************************
     C*****              eval      scTrnHead1 = 'A/R batch type '
     C*****                                   + '. . . . . . . . . :'
     C                   eval      scTrnHead1 = 'A/R batch type '
     C                                        + '. . . . . . . . . . . :'
      ******* end chg ****** 08/30/10 *********************************
     C                   select
     C                   when      WCFL24 = 'Y'
      ******* beg chg ****** 08/30/10 *********************************
     C*******            eval      scBchTypD1 = '(A=ACH, L=Lockbox,'
     C*******            eval      scBchTypD2 = ' S=S/R Upload, blank=Regular)'
      ******* beg chg ****** 04/21/11 *********************************
     C*******            eval      type_desc1 = '(A=ACH, L=Lockbox,'
     C                   eval      type_desc1 = '(A=EFT, L=Lockbox,'
      ******* end chg ****** 04/21/11 *********************************
     C                   eval      type_desc2 = ' S=S/R Upload, blank=Regular)'
      ******* end chg ****** 08/30/10 *********************************
     C                   eval      scTrnHead2 = headAL
     C                   other
      ******* beg chg ****** 08/30/10 *********************************
     C*******            eval      scBchTypD1 = '(L=Lockbox,'
     C*******            eval      scBchTypD2 = ' blank=Regular)'
     C                   eval      type_desc1 = '(L=Lockbox,'
     C                   eval      type_desc2 = ' blank=Regular)'
      ******* end chg ****** 08/30/10 *********************************
     C                   eval      scTrnHead2 = headL
     C                   endsl
     C*** BEGIN ADD ****** 08/09/10 ************************************
     C*** beg chg ***** 09/07/10 ***************************************
     c***********        if        cltid = '071' and @@mode = 'C'
     C                   if        cltid = '071'
     C*** end chg ***** 09/07/10 ***************************************
      ******* beg chg ****** 08/30/10 *********************************
     C*******            eval      scBchTypD1 = %trim(scBchTypD1) + ' R=RDP,'
     C                   eval      type_desc1 = %trim(type_desc1) + ' R=RDP,'
      ******* end chg ****** 08/30/10 *********************************
     C                   endif
     C*** END   ADD ****** 08/09/10 ************************************
     C*
     C                   eval      useDed = WCFL06 = 'Y'
     C                   eval      useLargeDed = WCFL43 = 'Y'
     C***  End   Add  ***  03/25/10  **************************************

     C***  Begin Add  ***  11/01/05  **************************************
     C                   select
     C                   when      cltID = '109'
     C***  Begin Del  ***  03/25/10  **************************************
     C**********         eval      scTrnHead1 = head109
     C***  End   Del  ***  03/25/10  **************************************
     C                   eval      scTrnHead2 = head109
     C                   other
     C***  Begin Del  ***  03/25/10  **************************************
     C**********         eval      scTrnHead1 = headAL
     C**********         eval      scTrnHead2 = headL
     C***  End   Del  ***  03/25/10  **************************************
     C                   endsl
     C***  End   Add  ***  11/01/05  **************************************
      *
     C                   SETOFF                                       40
      //*** Beg   Add *** 07/29/22 *************************************
        //   vis_ACHF = *Off;
      //*** End   Add *** 07/29/22 *************************************
      *
     C     @@MODE        IFEQ      'A'
     C     @@MODE        OREQ      'P'
     C                   SETON                                        40
      //*** Beg   Add *** 07/29/22 *************************************
        // vis_ACHF = *On;
      //*** End   Add *** 07/29/22 *************************************
     C                   ENDIF
      *

     C                   SETOFF                                       59
     C     @@RRN         CHAIN(N)  WDEAP                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      EABAT#        SCBAT#
      //*** Begin Add *** 05/24/21 *************************************
      //*** Begin Add *** 11/06/28 *************************************
                         If @@mode <> 'D';
      //*** End   Add *** 11/06/28 *************************************
                         KeyWXRIL3.RInam  = 'WDEAP';
                         KeyWXRIL3.RIco#  = EAco#;
                         KeyWXRIL3.RIbat# = EAbat#;
                         Monitor;
                            Chain %kds(KeyWXRIL3) PRI01;    //Lock it
                            If not %found(WXRIL3);
                               Clear PRI01;
                               RIstat = 'A';
                               RInam = 'WDEAP';
                               RIrrn = @@rrn;
                               //*** Beg Del *** 11/06/24 *************
                               //*** RIfnd = EAfnd;
                               //*** End Del *** 11/06/24 *************
                               RIco# = EAco#;
                               //*** Beg Del *** 11/06/24 *************
                               //*** RIemp# = EAemp#;
                               //*** End Del *** 11/06/24 *************
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
                               Chain %kds(KeyWXRIL3) PRI01; //Lock it
                            Endif;
                         On-Error;
                         EndMon;
      //*** Begin Add *** 11/06/28 *************************************
                         Endif;
      //*** End   Add *** 11/06/28 *************************************
      //*** End   Add *** 05/24/21 *************************************
     C*** BEGIN ADD ****** 08/09/10 ************************************
     C*** beg chg ***** 09/07/10 ***************************************
     C*****@@MODE        IFEQ      'C'
     C*****EAACHF        ANDEQ     'R'
     C     EAACHF        IFEQ      'R'
     C*** end chg ***** 09/07/10 ***************************************
     C                   SETON                                        59
     C                   ENDIF
     C*** END   ADD ****** 08/09/10 ************************************
     C                   ENDIF
      *
     C  N09EASTAT        COMP      'T'                                    09
     C*** Begin add *** 02/22/05 **************************************
     C                   exsr      chksec
     C*** End   add *** 02/22/05 **************************************
      ******* beg del ****** 02/15/11 *********************************

     c                   if        wcfl44 = 'Y'
      ******* beg chg ****** 05/20/11 *********************************
     c*******            eval      type_desc1 = %trim(type_desc1) + ' F=Fee,'
     c                   eval      type_desc1 = %trim(type_desc1)
      ******* end chg ****** 05/20/11 *********************************
     c                   endif

      ******* end del ****** 02/15/11 *********************************
      *
     C                   ENDSR                                                  PGMSTR


      *================================================================
      * PREOTH - Pre-processing for other modes
      *================================================================
     C     PREOTH        BEGSR
      *
     C                   SELECT
      *
     C     @@MODE        WHENEQ    'W'
     C     @@MODE        OREQ      'X'
     C                   EXSR      UPDATR
           //*** Begin Add *** 11/06/24 **********************
           delete pri01;
           //*** End   Add *** 11/06/24 **********************
     C                   RETURN
     C***  begin add  ***  04/07/05 ***********************************
     C     @@MODE        WHENEQ    'U'
     C                   EXSR      MASSUP
     C                   RETURN
     C***  end   add  ***  04/07/05 ***********************************
     C                   ENDSL
      *
     C                   ENDSR
      *
      *================================================================
      * UPDATR - Update data base record and after processing
      *================================================================
     C     UPDATR        BEGSR
      *
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
      *
     C                   SELECT
      *
     C     @@MODE        WHENEQ    'W'
      *
     C     EATFAP        IFEQ      'Y'
     C                   MOVE      'N'           EATFAP
     C                   ELSE
     C                   MOVE      'Y'           EATFAP
     C                   ENDIF
      *
     C     @@MODE        WHENEQ    'X'
      *
     C     EATFFL        IFEQ      'Y'
     C                   MOVE      'N'           EATFFL
     C                   ELSE
     C                   MOVE      'Y'           EATFFL
     C                   ENDIF
      *
     C                   OTHER
     C                   EXSR      MVSCDB
     C                   ENDSL
      *
      *** beg chg ***** 09/24/03 ***********************************************
     C*****************  EXSR      ZUPFLD
     C                   MOVE      Q1PGM         EAPGM
     C                   MOVE      Q1USER        EAUSER
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   MOVEL     DSTIME        EATIME
      *** end chg ***** 09/24/03 ***********************************************
     C                   UPDATE    DBRCDF
      *
     C     *LIKE         DEFINE    EABATA        SVBATA
     C     *LIKE         DEFINE    EATFAP        SVTFAP
     C     *LIKE         DEFINE    EATFFL        SVTFFL
     C     *LIKE         DEFINE    EASTAT        SVSTAT
     C                   MOVE      EABATA        SVBATA
     C                   MOVE      EATFAP        SVTFAP
     C                   MOVE      EATFFL        SVTFFL
     C                   MOVE      EASTAT        SVSTAT
      ***** Begin Add  **** 04/16/03 ***************************************
     C                   MOVE      EATRDT        SVTRDT
      ***** End   Add  **** 04/16/03 ***************************************
     C     EAKY1         CHAIN     WDEAL4                             99
     C     *IN99         DOWEQ     *OFF
     C                   MOVE      SVBATA        EABATA
     C                   MOVE      SVTFAP        EATFAP
     C                   MOVE      SVTFFL        EATFFL
     C                   MOVE      SVSTAT        EASTAT
      ***** Begin Add  **** 04/16/03 ***************************************
     C*** BEGIN ADD ****** 08/09/10 ************************************
     C* Prevent EATRDT from being updated for retro batches.
     C                   IF        EAACHF <> 'R'
     C*** END   ADD ****** 08/09/10 ************************************
     C                   MOVE      SVTRDT        EATRDT
     C*** BEGIN ADD ****** 08/09/10 ************************************
     C                   ENDIF
     C*** END   ADD ****** 08/09/10 ************************************
      ***** End   Add  **** 04/16/03 ***************************************
      *
      ***** BEG ADD ***** 09/07/04 *************************************
      *** beg del ***** 10/07/05 ***************************************
     C*********          If        *IN39
     C*********          move      'Y'           EACASH
     C*********          ELSE
     C*********          eval      EACASH = ' '
     C*********          EndIF
      *** end del ***** 10/07/05 ***************************************
      ***** END ADD ***** 09/07/04 *************************************
      *
     C                   UPDATE    PEA01
      *
     C     EAKY1         READE     WDEAL4                                 99
      *
     C                   ENDDO
      *
E01  C                   ENDIF
      *
     C                   ENDSR
      *================================================================
      * WRITER - Write data base record and after processing
      *================================================================
     C     WRITER        BEGSR
      *
      *  Determine last used batch number
      *
     C     *LIKE         DEFINE    EABAT#        NNBAT#
     C                   SUB       NNBAT#        NNBAT#
     C                   MOVE      *HIVAL        EABAT#
     C     EAKY1         SETGT     WDEAL4
     C     ##CO#         READPE(N) WDEAL4                                 99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     EABAT#        NNBAT#
     C                   ENDIF
      *
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C     NNBAT#        ADD       1             EABAT#
     C                   MOVE      ##CO#         EACO#
     C                   MOVE      *HIVAL        EACHK#
     C                   MOVE      *HIVAL        EACKAM
     C                   MOVE      'N'           EATFAP
     C                   MOVE      'N'           EATFFL
     C                   EXSR      ZUPFLD
     C                   WRITE     DBRCDF
       //*** Begin Add *** 05/24/21 ***********************************
              Clear PRI01;
              RIstat = 'A';
              RInam = 'WDEAP';
              RIrrn = dbrrn;
              //*** Beg Del *** 11/06/24 *************
              //*** RIfnd = EAfnd;
              //*** End Del *** 11/06/24 *************
              RIco# = EAco#;
              //*** Beg Del *** 11/06/24 *************
              //*** RIemp# = EAemp#;
              //*** End Del *** 11/06/24 *************
              RIbat# = EAbat#;
              RIjobn = Q1jobn;
              RIJOB =  Q1job;
              RIdate = Q1dt;
              RItime = Q1time;
              RIipgm = Q1pgm;
              RIpgm  = Q1pgm;
              //*** Beg Add *** 11/06/24 **************
              RIuser = Q1user;
              //*** End Add *** 11/06/24 **************
              write PRI01;
       //*** End   Add *** 05/24/21 ***********************************
      *
     C                   CALL      'W1210R'                             99
     C                   PARM                    ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      DBRRN         ##RRN             9 0
      *
     C                   FEOD      WDEAP
     C                   MOVE      *ON           OUTFLG
     C                   ENDSR
      *================================================================
      * ZUPFLD - Update program, user, date/time fields
      *================================================================
     C     ZUPFLD        BEGSR
      *
     C                   MOVE      Q1PGM         EAPGM
     C                   MOVE      Q1USER        EAUSER
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   MOVEL     DSTIME        EATIME
      *** beg add ***** 09/24/03 **************************************
     C                   MOVE      Q1PGM         EAIPGM
     C                   MOVE      Q1USER        EAIUSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAIDT
      *** end add ***** 09/24/03 **************************************
      *
     C                   ENDSR
