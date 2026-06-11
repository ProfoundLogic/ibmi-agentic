     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *****************************************************************
     H/TITLE ISEWMD - EDIT/WRITE MD RECORD FOR VIS DARTS
      **************************************************************   *******/
      * Revisions:                                                           */
      *
      *  09/06/13 C:33  Project; SY195  #112
      *                 Ensure path is correct for DataCenter.
      *
      *  07/25/13 C:295 Support 82807
      *                 Incorrect UNC path
      *
      *  02/04/13 C:191 PROJECT SY195 Redmine #2231
      *                 Add root for UNCN if blanks
      *                                                                      */
      *  11/16/12 C:282 PROJECT SY195 Priss 88
      *                Update the path for data center client.
      *                                                                      */
      *  10/11/12 C:191 Project: SY195 PRISS
      *                 USE QQRPYN IF PASSED IN
      *                                                                      */
      *  10/17/11 C:279 Project: SY195
      *                 Modify program to pass/return UNC path field
      *                 and IFS flag.
      *
      * 01/14/11  C:33  Project: WC1838 (132WMO Priss #42)                   */
      *                 Check for the prior existance of PDS/xxx/ in         */
      *                  the folder name and edit out.  Follow DA700R.       */
      *                                                                      */
      * 11/02/10 C:183 Project : SY195                                       */
      *                Add two more parms for the path and whether to        */
      *                add root directory (Y/N). Retrieve the document       */
      *                name and folder name from the path and save it        */
      *                to MDOFLR and MDOID.                                  */
      *                                                                      */
      * 08/16/02  C:128  Project: VI115  Support: #33754                     */
      *           Change Document ID to upper case for write to SMMDL.       */
      *****************************************************************
      * FILE SPECIFICATIONS  * * *
      *****************************************************************
      * INDICATOR USAGE
      * ALL/DIFFERENT SCREENS AND FUNCTIONS:
      *  60      GENERAL CHAIN: ALL
      *****************************************************************
      *
     FSMMDL     IF A E           K DISK
     FSSYLF     IF   E           K DISK
     FSDCMLA    UF   E           K DISK
     FIMIFL2    IF   E           K DISK    USROPN
     FSAIFL2    IF   E           K DISK    USROPN
     F                                     RENAME(PIF01:PIF02)
     FIMDOLB    IF   E           K DISK    USROPN
      *
      *****************************************************************
      * EXCEPTION SPECIFICATIONS * * *
      *****************************************************************
      *

     D******************* MP         79  1               KEY DEF TO DSP ARRY
     D*
      *** Beg add ***** 11/02/10 ***********************************************
     D AL              S             64    DIM(1) CTDATA PERRCD(1)
      *** End add ***** 11/02/10 ***********************************************
      *****************************************************************
      * INPUT SPECIFICATIONS  * * *
      *****************************************************************
      *
      *** Beg add ***** 11/02/10 ***********************************************
     D RPGM            C                   CONST('RTVDOCROOT')
     D RPGM1           C                   CONST('RTVDOCENV')
      *** End add ***** 11/02/10 ***********************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D***  Begin Add  ***  01/14/11  **************************************
     D                 DS
     D  TFOLD                  1     80
     D  TEST                   1      4
     D  FOLDER                 9     80
     D***  End   Add  ***  01/14/11  **************************************
     D*
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*
     D EXIST           C                   CONST('This object already -
     D                                     exists for this Key/-
     D                                     Seq#.  Cannot Add.  ')
     D*
     D NOIMG           C                   CONST('Imaging is not insta-
     D                                     lled.  Cannot use Ob-
     D                                     j type "I".         ')
     D*
     D NOFAX           C                   CONST('Fax is not installed-
     D                                     . Cannot use Obj Typ-
     D                                     e "F".              ')
     D*
     D*
     D NOPGM           C                   CONST('Fax/Image Database c-
     D                                     ould not be updated.-
     D                                      Contact your system-
     D                                      administrator.     ')
     D*
     D*
     D FAXRCD          C                   CONST('Object must exist as-
     D                                      a fax on the Server-
     D                                      before you can Add/-
     D                                     Link as a "F" type. ')
     D*
     D*
     D*
     D IMGRCD          C                   CONST('Object must exist as-
     D                                      an image on the Ser-
     D                                     ver before you can A-
     D                                     dd/Link as type "I".')
     D*
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
      *** Begin Change 08/16/02 ***************************************
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
      *** End   Change 08/16/02 ***************************************
     D***  Begin Add  ***  09/06/13  **************************************
     DstrFlrPrefix     s              8
     D***  End   Add  ***  09/06/13  **************************************

     I*
      *****************************************************************
      * CALCULATION SPECIFICATIONS  * * *
      *****************************************************************
      *

     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
     C*
     C*   KEY LIST FOR THE MULTI-DOC (OBJECT) FILE
     C*
     C     MDKY          KLIST
     C                   KFLD                    QQKEY            40
     C                   KFLD                    QQSEQ#            5 0
     C                   KFLD                    QQOID            40
     C***  Begin Chg  ***  01/14/11  **************************************
     C**********           KFLD           QQOFLR 60
     C                   KFLD                    XXOFLR
     C***  End   Chg  ***  01/14/11  **************************************
     C*
     C     CMKY          KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    QQSEQ#
     C*
     C*
     C*
     C     DOKY          KLIST
     C                   KFLD                    XXDCID           12
     C                   KFLD                    XXFLD             8
     C                   KFLD                    XXSFLR           12
     C*
     C*
     C     IFKY          KLIST
     C                   KFLD                    XXDCID           12
     C*
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQKEY
     C                   PARM                    Q#SEQ#            5
     C                   PARM                    QQOID
     C***  Begin Chg  ***  01/14/11  **************************************
     C**********           PARM           QQOFLR
     C                   PARM                    QQOFLR           60
     C***  End   Chg  ***  01/14/11  **************************************
     C                   PARM                    QQOTYP            1
     C                   PARM                    QQODSC           40
     C                   PARM                    QQRMCD            5
     C                   PARM                    QQSYSN           20
     C                   PARM                    QQPNAM           10
     C                   PARM                    QQUSR            10
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQEDSC           80
     C*** Beg add ***** 11/02/10 **************************************
     C                   PARM                    QQPATH           80            40
     C                   PARM                    QQRPYN            1            41 (Add Root
      *                                                    to Path Y/N)
     C*** End add ***** 11/02/10 **************************************
     C*** Beg add ***** 10/17/11 **************************************
     C                   PARM                    QQUNCN          255
     C                   PARM                    QQIFSP            1
     C*** End add ***** 10/17/11 **************************************
     C*
     C***  Begin Add  ***  01/14/11  **************************************
     C     *LIKE         DEFINE    QQOFLR        XXOFLR
     C                   MOVEL(P)  QQOFLR        TFOLD
     C                   SELECT
     C     TEST          WHENEQ    'PDS/'
     C                   MOVEL(P)  FOLDER        XXOFLR
     C                   OTHER
     C                   MOVEL(P)  QQOFLR        XXOFLR
     C                   ENDSL
     C***  End   Add  ***  01/14/11  **************************************
     C*
     C                   MOVE      Q#SEQ#        QQSEQ#
     C*
     C                   MOVEL     'SY'          XXSYK
     C*
      *** ??????????? DO I NEED TO UPPER CASE ALL OID/OFLR/KEY INFO?
     C*
      *** Begin Change 08/16/02 ***************************************
     C                   MOVE      QQOID         HOLD40           40
     C     LO:UP         XLATE     HOLD40        QQOID
      *** End   Change 08/16/02 ***************************************
     C     SYKY          CHAIN     SSY01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     SYFXIF        FAX               1
     C                   MOVEL     SYIMIF        IMAGE             1
     C                   ELSE
     C                   MOVEL     'N'           FAX               1
     C                   MOVEL     'N'           IMAGE             1
     C                   ENDIF
     C*
     C                   MOVEL     *BLANK        QQRTCD
     C                   MOVEL     *BLANK        QQEDSC
     C*
     C     MDKY          CHAIN     PMD01                              60
     C*
     C*** IF THE RCD ALREADY EXISTS, END THIS PROGRAM WITH MSG ***
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      '1'           QQRTCD
     C                   MOVEL     EXIST         QQEDSC                          ** EXISTS ALRDY
     C                   GOTO      ENDPGM
     C                   ENDIF
     C*
      ***==============================================================***
      * IF "I" OR "F" FOR OBJECT TYPE ...
      * CHECK TO SEE IF IMAGE/FAX IS INSTALLED.  IF NOT, THEN CANNOT USE
      * AN "I" OR "F" FOR THE OBJECT TYPE.  MUST USE "O" OR "D".
      *
     C     QQOTYP        IFEQ      'I'
     C     IMAGE         ANDNE     'Y'
     C                   MOVEL     '2'           QQRTCD                          *** NO IMAGE INS
     C                   MOVEL     NOIMG         QQEDSC                          ** NO IMAGE
     C                   GOTO      ENDPGM
     C                   ENDIF
     C     QQOTYP        IFEQ      'F'
     C     FAX           ANDNE     'Y'
     C                   MOVEL     '3'           QQRTCD                          *** NO IMAGE INS
     C                   MOVEL     NOFAX         QQEDSC                          ** NO IMAGE
     C                   GOTO      ENDPGM
     C                   ENDIF
      *
      ***** VALIDATA THE IMAGE/FAX INFO *********
      *
     C     QQOTYP        IFEQ      'I'
     C                   OPEN      IMDOLB
     C*
     C                   MOVEL     QQOID         XXDCID
     C***  Begin Chg  ***  01/14/11  **************************************
     C**********           MOVELQQOFLR    XXFLD
     C********** 12        SUBSTQQOFLR:10 XXSFLR
     C                   MOVEL     XXOFLR        XXFLD
     C     12            SUBST     XXOFLR:10     XXSFLR
     C***  End   Chg  ***  01/14/11  **************************************
     C*
     C     DOKY          CHAIN     PDO01                              66
     C     *IN66         IFEQ      *ON
     C                   MOVEL     '6'           QQRTCD                          *** NO IMG rcd S
     C                   MOVEL     IMGRCD        QQEDSC                          ** on 400.
     C                   GOTO      ENDPGM
     C                   ELSE
     C     DOFL01        IFNE      'Y'
     C     DOFL01        ANDNE     'I'
     C     DOFL01        ANDNE     *BLANK
     C                   MOVEL     '6'           QQRTCD                          *** NO IMG rcd S
     C                   MOVEL     IMGRCD        QQEDSC                          ** on 400.
     C                   GOTO      ENDPGM
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*
     C     QQOTYP        IFEQ      'F'
     C                   OPEN      SAIFL2
     C                   MOVEL     QQOID         XXDCID
     C*
     C     IMAGE         IFEQ      'Y'
     C                   OPEN      IMIFL2
     C     IFKY          CHAIN     PIF01                              66         'IMIFL2
     C     *IN66         IFEQ      *ON
     C                   MOVEL     '5'           QQRTCD                          *** NO fax rcd S
     C                   MOVEL     FAXRCD        QQEDSC                          ** on 400.
     C                   GOTO      ENDPGM
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     IFKY          CHAIN     PIF02                              66
     C     *IN66         IFEQ      *ON                                           'SAIFL2
     C                   MOVEL     '5'           QQRTCD                          *** NO fax rcd S
     C                   MOVEL     FAXRCD        QQEDSC                          ** on 400.
     C                   GOTO      ENDPGM
     C                   ENDIF
     C*
     C                   ENDIF
      *
      *
     C                   TIME                    DSDTTM
      *
     C                   MOVEL     QQKEY         MDKEY
     C                   MOVE      QQSEQ#        MDSEQ#
     C                   MOVEL     QQOID         MDOID
     C***  Begin Chg  ***  01/14/11  **************************************
     C***********          MOVELQQOFLR    MDOFLR
     C                   MOVEL     XXOFLR        MDOFLR
     C***  End   Chg  ***  01/14/11  **************************************
     C                   MOVEL     QQOTYP        MDOTYP
     C                   MOVEL     QQODSC        MDODSC
     C                   MOVEL     *BLANK        MDDOCT
      *** TRDT/TRTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MDTRDT
     C                   MOVE      DSTIME        MDTRTM
     C*
     C*
     C                   MOVEL     'A'           MDSTAT
     C*
     C                   MOVEL     *BLANK        MDCSID
     C                   MOVEL     *BLANK        MDCODE
     C                   MOVEL     QQPNAM        MDPNAM
     C                   MOVEL     *ZEROS        MDSECL
     C                   MOVEL     *BLANK        MDSBYL
     C                   MOVEL     QQRMCD        MDRMCD
      *** IUSR/IPGM/ITIM/IDT/LUSR/LPGM/LDT/LTIM
     C                   MOVEL     QQUSR         MDIUSR
     C                   MOVEL     Q1PGM         MDIPGM
     C                   MOVEL     QQUSR         MDLUSR
     C                   MOVEL     Q1PGM         MDLPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MDIDT
     C                   MOVE      DSTIME        MDITIM
     C*
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MDLDT
     C                   MOVE      DSTIME        MDLTIM
     C*
     C*
     C                   MOVEL     QQSYSN        MDSYSN
     C*
     C*** Beg add ***** 11/02/10 **************************************
     C*** Beg Add ***** 10/17/11 **************************************
      * Get Root
     C                   CALL      RPGM
     C                   PARM                    ROOT             10
      * Find IFS or QDLS
     C                   CALL      RPGM1
     C                   PARM                    ENV               1
     C*** End Add ***** 10/17/11 **************************************
     C*** Beg Del ***** 10/17/11 **************************************
     C*****      Q1PRMS    IFGT 40
      * Get Root
     C*****                CALL RPGM
     C*****                PARM           ROOT   10
      * Find IFS or QDLS
     C*****                CALL RPGM1
     C*****                PARM           ENV     1
      *
     C*****                MOVE ENV       MDUNCF
     C*** End Del ***** 10/17/11 **************************************
      *** Beg Add ***** 11/16/12 **************************************
     C                   Call      'SA030R'
     C                   Parm      *BLANKS       QQSUFX            3
     C                   Parm      *BLANKS       QQFLRP           80
      *
     C                   Movel     *BLANKS       XXFLRP           81
     C                   Movel     *BLANKS       XXSLASH           1
     C                   If        QQFLRP <> *BLANKS
     C                   Eval      XXSLASH = '/'
     C                   Eval      XXFLRP  = %trim(QQFLRP) + '/'
     C***  Begin Add  ***  09/06/13  **************************************
      /Free
       strFlrPrefix = '/' + %trim(qqFlrP);
      /End-Free
     C***  End   Add  ***  09/06/13  **************************************
     C                   EndIf
      *** END Add ***** 11/16/12 **************************************
     C*** Beg Add ***** 10/11/12 **************************************
     C                   MOVE      *BLANKS       XXRPYN            1
     C*** End Add ***** 10/11/12 **************************************
     C*** Beg Add ***** 10/17/11 **************************************
     C     Q1PRMS        IFGT      12
     C*** Beg Add ***** 10/11/12 **************************************
     C                   MOVE      QQRPYN        XXRPYN
     C*** End Add ***** 10/11/12 **************************************
     C*** End Add ***** 10/17/11 **************************************
      * If the path is not blanks and the document and folder passed are blanks
     C     QQPATH        IFNE      ' '
     C     QQOID         ANDEQ     ' '
     C     QQOFLR        ANDEQ     ' '
      *
      * This step will get you the position of last '/' in the path.
     C     AL(1)         CHECKR    QQPATH        SST               5 0
      *
     C***  Begin Chg  ***  09/06/13  **************************************
     C*****QQRPYN        IFEQ      'Y'
     C**************       MOVELROOT      MDUNCP
     C*****'/'**         CAT       ROOT          MDUNCP
      ****Beg*Add ***** 11/16/12 ***********************************************
     C*****MDUNCP        CAT       XXSLASH:0     MDUNCP
     C*****MDUNCP        CAT       XXFLRP:0      MDUNCP
      ****End*Add ***** 11/16/12 ***********************************************
     C*****MDUNCP        CAT       QQPATH:0      MDUNCP
     C**********         ELSE
     C**********         MOVEL     QQPATH        MDUNCP
     C**********         ENDIF                                                  QQRPYN IFEQ 'Y'
      /Free
       Select;
       When qqRpYN = 'Y';
          MDuncP = '/' + %trim(root) + %trim(strFlrPrefix) + '/' +
           %trim(qqPath);
       Other;
          MDuncP = qqPath;
       Endsl;
      /End-free
     C***  End   Chg  ***  09/06/13  **************************************
     C*** Beg Add ***** 10/17/11 **************************************
     C                   MOVE      ENV           MDUNCF
     C*** End Add ***** 10/17/11 **************************************
      *
      * Subtract 1 to get the position of the character before last '/'.
     C                   SUB       1             SST
     C     SST           SUBST     QQPATH:1      MDOFLR
      *
      * Add 2 to get the position of the character after last '/'.
     C                   ADD       2             SST
     C                   SUBST     QQPATH:SST    MDOID
      *
     C                   ENDIF                                                  QQPATH IFNE ' '
      *
     C                   ENDIF                                                  Q1PRMS IFGT 12
     C*** End add ***** 11/02/10 **************************************
     C*** Beg add ***** 10/17/11 **************************************
      * Get UNCN if received in entry
     C     Q1PRMS        IFGT      14
     C     QQUNCN        ANDNE     ' '
      *
     C                   MOVEL     QQUNCN        MDUNCP
     C                   MOVEL     ENV           MDUNCF
     C                   ENDIF                                                  /* Q1PRMS>14 */
      * Get UNCN path if not created till this step.
     C     MDUNCP        IFEQ      *BLANKS
      *
      * Here we will create path based on folder and doc id.
     C***************      MOVEL'/'       MDUNCP
     C*** Beg CHG ***** 10/11/12 **************************************
     C********   QQRPYN    IFEQ 'Y'
     C*** Beg Del ***** 02/04/13 **************************************
     C***  XXRPYN        IFEQ      'Y'
     C*** End Del ***** 02/04/13 **************************************
     C*** Beg CHG ***** 10/11/12 **************************************
     C********** MDUNCP    CAT  ROOT:0    MDUNCP
     C**********           MOVELROOT      MDUNCP
     C***  Begin Chg  ***  09/06/13  **************************************
     C*****'/'**         CAT       ROOT          MDUNCP
     C*****MDUNCP        CAT       '/':0         MDUNCP
     C****Beg*Del ***** 02/04/13 **************************************
     C***********        ENDIF
     C****End*Del ***** 02/04/13 **************************************
      ****Beg*Add ***** 11/16/12 ***********************************************
      ****Beg*Del ***** 07/25/13 ***********************************************
     C*****MDUNCP        CAT       XXFLRP:0      MDUNCP
      ****End*Del ***** 07/25/13 ***********************************************
      ****End*Add ***** 11/16/12 ***********************************************
     C*****MDUNCP        CAT       QQOFLR:0      MDUNCP
     C*****MDUNCP        CAT       '/':0         MDUNCP
     C*****MDUNCP        CAT       QQOID:0       MDUNCP
      /Free
       If %subst(qqOFlr:1:8) = strFlrPrefix
        or %subst(qqOFlr:1:7) = %subst(strFlrPrefix:2:7);
          Clear strFlrPrefix;
       Endif;
       MDuncP = '/' + %trim(root) + %trim(strFlrPrefix) + '/' +
        %trim(qqOFlr) + '/' + %trim(qqOId);
      /End-free
     C***  End   Chg  ***  09/06/13  **************************************
     C                   MOVEL     ENV           MDUNCF
      *
     C                   ENDIF                                                  /* if MDUNCP = Blank
     C*** End add ***** 10/17/11 **************************************
      ***
      ***
     C                   WRITE     PMD01
     C*
      *
      *
      *
      ***==============================================================***
     C*
     C     CMKY          CHAIN     PCM01                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVE      'Y'           CMQPTY
     C                   UPDATE    PCM01
     C                   ENDIF
     C*
     C*
     C     ENDPGM        TAG
     C**** KEEP PGM ACTIVE BY USING RETURN... HELP SPEED OF PROCESS.
     C******************** RETRN *****************************************
     C     ENDIT         TAG
     C                   MOVE      '1'           *INLR
**
ABCDEFGHIJKLMNOPQRSTabcdefghijklmnopqrstuvwxyz .0123456789      /* 11/02/10 */
