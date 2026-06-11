     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      ************************************************************************/
     * StoneRiver is a trademark of StoneRiver, Inc.                 *
      ************************************************************************/
      * SCI1WQR - PDS QUOTE    KEY, IMPORT PROCESSING PROGRAM                 */
      *                                                                      */
      * Description: CALLED FROM SCI105R - PROCESSES FILE IN IFS             */
      *                                                                      */
      * Created:                                                             */
      * Programmer:                                                          */
      * Project:                                                             */
      **************************************************************   *******/
      * Revisions:                                                           */
     *                                                                   *
     * 06/25/20  C:062 Project: 146DHK                                   *
     *                 Send email when error has occurred.               *
     *                                                                   *
     * 11/02/10  C:183 Project: SY195                                    *
     *                 Remove hard coding for QDLS. Call RTVDOCROOT to   *
     *                 get the root folder name.                         *
      *
      *    01/28/10 C:249 PROJECT: SY215
      *             C:189 CHANGE BRAND NAME.
      *
      *    05/30/08 C:62 PROJECT: DR257
      *                  ADD ROLLOVER PROCESSING ERROR MESSAGES.
      *
      ***  02 - 02/12/07 - CHANGES TO EXIT PROCESSING IMMED IF KEY IS NOT VALID
      *
      *    53 - 06/19/06 - CALL API DA002R TO WRITE LOG RECORDS.  REMOVE
      *                    OUTPUT FOR SUCESSFULL IMPORTS.
      *
      *   02  - 6/14/06 - FIND NEW DIRECTIVES IN THE FILE NAME.  ALLOW PROCESS
      *                   OF .IDX FILE WHICH WILL CONTAIN TEXT THAT DETERMINES
      *                   FILE ATTACHMENTS.
      *
      *"RD_" WILL BE THE IDENTIFIER THAT DESCRIBES THE REMARK DATE IN THE FILE N
      *AME. A PRECEDING BLANK IS NECESSARY, AND A BLANK WILL FOLLOW THE DATE SPE
      *CIFIED.  DATE WILL BE IN MMDDYYYY FORMAT ONLY.  INVALID DATE WILL DEFAULT
      *TO CURRENT DATE.
      *
      *"RC_" WILL BE THE IDENTIFIER IN THE FILE NAME THAT DESCRIBES THE REMARK C
      *ODE TO USE.  A PRECEDING BLANK IS NECESSARY, AND A BLANK WILL FOLLOW THE
      *REMARK CODE SPECIFIED.   INVALID REMARK CODE WILL DEFAULT TO THE REMARK C
      *ODE SETUP ON THE OBJECT IMPORT SETUP SCREEN.
     F*
     F*"DD_" WILL BE THE IDENTIFIER THAT DESCRIBES THE DIARY DATE IN THE FILE NA
      *ME. A PRECEDING BLANK IS NECESSARY, AND A BLANK WILL FOLLOW THE DATE SPEC
      *IFIED.  DATE WILL BE IN MMDDYYYY FORMAT ONLY.  INVALID DATE WILL DEFAULT
      *TO CURRENT DATE.
      *
      *"DU_" WILL BE THE IDENTIFIER THAT DESCRIBES THE DIARY USER IN THE FILE NA
      *ME. A PRECEDING BLANK IS NECESSARY, AND A BLANK WILL FOLLOW THE USERID SP
      *ECIFIED.  SPECIAL VALUES CAN BE USED (SEE DESCRIPTIONS ABOVE) AS WELL.  I
      *NVALID VALUE WILL CAUSE THE REMARK NOT TO BE DIARIED.
      *
      *"DS_" WILL BE THE IDENTIFIER THAT DESCRIBES THE DESCRIPTION IN THE FILE N
      *AME. A PRECEDING BLANK IS NECESSARY, AND A BLANK WILL FOLLOW THE DESCRIPT
      *ION SPECIFIED.  THE IDENTIFIER WAS NOT NECESSARY BEFORE, BUT DUE TO THE N
      *EW VALUES WE'RE EXTRACTING, WE NEED A WAY TO IDENTIFY THE INTENTION OF AL
      *L PARTS OF THE FILE NAME.  DESCRIPTION WILL WORK AS IT DID ORIGINALLY, AN
      *D IT MUST BE THE LAST IDENTIFIER USED(IF USED) PRIOR TO THE "." (PERIOD)
      *FOR THE FILE EXTENSION.
      *
      *    02 - 06/13/06 - CHANGE TO ALSO PROCESS *DSTMF (QNTC PROCESSING)   *
      *
      *
      *
      *    02 - 08/30/05 - CHANGE TO GET THE WSID USED BY THE PROCESS FROM A
      *                    DATAAREA (OI).  WILL EXTEND THE LIMIT ON # OF OBJ
      *                    IMPORTED.
      *
      * 06/21/04 C:062 PROJECT: SY100
      *                Data Center Processing
      *
      * 06/14/04 C:062 PROJECT: 129WCS
      *                Add process to allow both a data entry pgm
      *                and reference an image
      *
      *    02 - 02/27/03 - CHANGE FILE COPY TO ARCHIVE AND ERROR TO INC.
      *                    DATE/TIME STAMP AS PART OF OBJECT NAME.
      *                                                                      */
      *                                                                      */
      * -------------------------------------------------------------------- */
      *  Note: Please add comments to the top of revisions.                  */
      * -------------------------------------------------------------------- */
      ***************************************************************  *******/
      *
     FSXFLP     IF   E           K DISK
     FSMI1P     IF   E             DISK
     F*** BEGIN ADD *** 06/14/04 **************************************
     FSTCTL1    IF   E           K DISK
     F*** END   ADD *** 06/14/04 **************************************
     F*** BEGIN ADD *** 06/14/06 **************************************
     FSMIMP     IF   E             DISK    USROPN
     FWMEML     IF   E           K DISK
     FWMUPL1    IF   E           K DISK
     FWMUPL     IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     F*** Begin Add *** 06/25/20 **************************************
     FSMERL     IF   E           K DISK    Rename(per01:per00)
     F*** End   Add *** 06/25/20 **************************************
     F*** END   ADD *** 06/14/06 **************************************
     FQSYSPRT1  O    F  198        PRINTER OFLIND(*INOA)
     F                                     USROPN
      *** BEG DEL *** 06/19/06 ****************************
     F*QSYSPRT2O***F     198     OB     PRINTER                        UC
      *** END DEL *** 06/19/06 ****************************
     F*  QSYSPRT1 IS THE REPROT FOR ERRORS, QSYSPRT2 IS THE REPORT FOR
     F*    SUCCESSFUL

     D*** Begin Add *** 06/25/20 *******************************************
     DGetSuffix        pr                  Extpgm('SA030R')
     D outSuffix                      3
     D outFolderPrfx                 80

     DSendEmail        pr                  Extpgm('SNDEMAIL')
     D inFromEmail                   60
     D inToEmail                    600
     D inSubject                    200
     D inBody                     10000
     D inAttach                    2870
     D inDarts                             likeds(dsDarts) options(*varsize)
     D inDataQ                             likeds(dsDataQ)
     D inAuto                         1
     D inProgram                     10
     D inIncJobInfo                   1n
     D inSuffix                       3

     DdsDataQ          ds                  qualified
     D object                        10    Inz('*DEFAULT')
     D library                       10    Inz(' ')

     DdsDARTS          ds                  qualified
     D dsKey                         40    overlay(dsDARTS) Inz(' ')
     D dsKeyType                      2    overlay(dsDARTS:*next) Inz(' ')
     D dsRemarkCode                   5    overlay(dsDARTS:*next) Inz(' ')
     d dsUser                        10    overlay(dsDARTS:*next)
     d                                     Inz('*CURRENT')

     d emailRecDs      ds           600
     d emailRec                      60    Dim(10) Overlay(emailRecDs:1)

     D outSuffix       s              3    Inz(' ')
     D outFolderPrfx   s             80    Inz(' ')

     D inFromEmail     s             60    Inz('CPSupport@stoneriver.com')
     D inToEmail       s            600    Inz(' ')
     D inSubject       s            200    Inz(' ')
     D inBody          s          10000    Inz(' ')
     D inAttach        s           2870    Inz(' ')
     D inAuto          s              1    Inz('Y')
     D inProgram       s             10    Inz(' ')
     D inIncJobInfo    s              1n   Inz(*On)
     D inSuffix        s              3    Inz(' ')
     D count           s              2  0 Inz(0)
     D*** End   Add *** 06/25/20 *******************************************
     D FND             S              3  0 DIM(132)                             FOUND ARRAY
     D R2              S              2    DIM(20)                              RTN CODES (2)ERR
     D R3              S              1    DIM(20)                              RTN CODES (3)SUCC
     D*** BEGIN ADD *** 06/14/06 **************************************
     D OVR             S              1    DIM(75) CTDATA PERRCD(75)            OVRDBF  COMMAND
     D*** END Add   *** 06/14/06 **************************************
      *
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
     D                 DS
     D  XXHKEY                 1     40
     D  #$CO#                  1      3
     D  #$FND                  4      6
     D  #$EMP#                 7     15
     D  #$DIV                 16     20
     D  #$CLR                 21     40
     D  #$Q                   40     40
     D*
      *
      *
     D*
     D                 DS
     D  NWKEY                  1     40
     D  #@CO#                  1      3
     D  #@FND                  4      6
     D  #@EMP#                 7     15
     D  #@DIV                 16     20
     D  #@BLNK                21     39
     D  #@Q                   40     40
     D*
     D*** BEGIN ADD *** 06/14/06 **************************************
     D*
     D                 DS
     D  @3ARMK                 1    256
     D  @3AR1                  1     75
     D  @3AR2                 76    150
     D  @3AR3                151    225
     D  @3AR4                226    256
     D*
     D @3FOBJ          DS
     D  FOBJ1                  1    256
     D  FOBJ2                257    512
     D  FOBJ3                513    768
     D  FOBJ4                769   1024
     D*
     D @3TOBJ          DS
     D  TOBJ1                  1    256
     D  TOBJ2                257    512
     D  TOBJ3                513    768
     D  TOBJ4                769   1024
     D*
     D*
     D*** END ADD ***** 06/14/06 **************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D** ADD 08/30/05 *******************************************************
     D OBJIMP         UDS
     D  X#WSID                 1      2
     D** END ADD 08/30/05 *****************************************************
     D*
     D @VR             C                   CONST('Error validating key-
     D                                      extracted from the -
     D                                     file name.          ')
     D*
     D*
     D @ER             C                   CONST('Error copying file t-
     D                                     o the Error director-
     D                                     y.                  ')
     D*
     D*
     D @DL             C                   CONST('Error deleting file -
     D                                     from the import dire-
     D                                     ctory.              ')
     D*
     D*
     D @DI             C                   CONST('Error Retrieving DI -
     D                                     and/or folder balanc-
     D                                     e info.  File not pr-
     D                                     ocessed.            ')
     D*
     D*
     D @QD             C                   CONST('Error copying file t-
     D                                     o QLDS file system. -
     D                                     File not processed. -
     D                                                         ')
     D*
     D*
     D @FB             C                   CONST('Error updating the f-
     D                                     ile and folder balan-
     D                                     ce info. File not pr-
     D                                     ocessed.            ')
     D*
     D*
     D @AR             C                   CONST('Error archiving the -
     D                                     file.               -
     D                                                         -
     D                                                         ')
     D*** BEGIN ADD *** 05/30/08 **************************************
     D*
     D*** BEGIN DEL *** 01/28/10 **************************************
     D***           'Document indexing ha-C         @RO
     D***           's rolled.  Please co-
     D***           'ntact Fiserv support-
     D***           '.                   '
     D*** END   DEL *** 01/28/10 **************************************
     D*** END   ADD *** 05/30/08 **************************************
     D*** BEGIN ADD *** 06/14/06 **************************************
     D*
     D @TX             C                   CONST('Error reading idx te-
     D                                     xt file. *SCI1DXC* T-
     D                                     X                   -
     D                                                         ')
     D*
     D*
     D @OP             C                   CONST('Error Opening idx te-
     D                                     xt file. *OPEN*  OP -
     D                                                         -
     D                                                         ')
     D*
     D*
     D @IP             C                   CONST('Error on contents of-
     D                                      the idx file.  Inva-
     D                                     lid Path specified. -
     D                                                         ')
     D*
     D*
     D @AD             C                   CONST('Error on contents of-
     D                                      the idx file.  *ADD-
     D                                     RMK Error writing ad-
     D                                     ditional Remarks.   ')
     D*
     D*
     D @ND             C                   CONST('Error on contents of-
     D                                      idx file.   No Vali-
     D                                     d data was found in -
     D                                     the contents.       ')
     D*
     D @A2             C                   CONST('Error on contents of-
     D                                      the idx file.  *ADD-
     D                                     RMK cant be prcsd w/-
     D                                     o main remark 1st.  ')
     D*
     D*
     D DLTOVR          C                   CONST('DLTOVR FILE(SMI-
     D                                     MP)')
     D*
     D VALIFS          C                   CONST('CHKIFSOBJ')
      *
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
      *** Beg add ***** 11/02/10 ***********************************************
     D RPGM            C                   CONST('RTVDOCROOT')
      *** End add ***** 11/02/10 ***********************************************

     C*
     C*
     **** EMPLOYER MASTER KEY ***
     C     EK            KLIST
     C                   KFLD                    @3CO#             3 0
     C                   KFLD                    @3FND             3 0
     C                   KFLD                    @3EMP#            9 0
     C                   KFLD                    @3DIV             5 0
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*** END   ADD *** 06/14/06 **************************************
     I*
     C*** BEGIN ADD *** 06/14/04 **************************************
     C*
     C     CTKY          KLIST
     C                   KFLD                    Q#RMCD
     C*** END   ADD *** 06/14/04 **************************************
      *
     C     FLKY          KLIST
     C                   KFLD                    @@TYPE           12
      *
      *
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*MDY'        SRFFMT            8
     C                   PARM      '*MDYY'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*** ADD 08/30/05 *******************************************************
     C*
     C     *DTAARA       DEFINE                  OBJIMP
      *
     C*** END ADD 08/30/05 ****************************************************
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@RRN             9 0
     C                   PARM                    RTNCD             2
     C*** BEGIN ADD *** 06/14/04 **************************************
     C                   CALL      'SA001R'
01   C                   PARM      *BLANKS       ##WCIF            1            WC INST
02   C                   PARM      *BLANKS       ##MCIF            1            MC INST
03   C                   PARM      *BLANKS       ##WCIN            1            WC INTERFACE
04   C                   PARM      *BLANKS       ##AR              1            MC AR INST
05   C                   PARM      *BLANKS       ##NTWK            1            NETWORK INST
06   C                   PARM      *BLANKS       ##UR              1            PRECERT INST
07   C                   PARM      *BLANKS       ##MED             1            MEDMGMT INST
08   C                   PARM      *BLANKS       ##EDI             1            EDI INST
09   C                   PARM      *BLANKS       ##WCDK            1            WC DARTS KEY
10   C                   PARM      *BLANKS       ##RHAB            1            REHAB INST
11   C                   PARM      *BLANKS       ##MSS             1            MS SUPPORT
12   C                   PARM      *BLANKS       ##FSCH            1            FEE SCHEDULE
13   C                   PARM      *BLANKS       ##PRVB            1            PROVIDER BILLS
14   C                   PARM      *BLANKS       ##NPWC            1            NON-PDS WC  LS
15   C                   PARM      *BLANKS       ##OS              1            OUTOME/SAVINGS
16   C                   PARM      *BLANKS       ##ARLS            1            Auto Release Flag
17   C                   PARM      *BLANKS       ##CCOD            1            Correct Coding Flag
18   C                   PARM      *BLANKS       ##CRVW            1            Code Review Flag
19   C                   PARM      *BLANKS       ##PROM            1            ProMatch Flag
20   C                   PARM      *BLANKS       ##MCRV            1            Medicare CR Flag
21   C                   PARM      *BLANKS       ##FREQ            1            Frequency Flag
22   C                   PARM      *BLANKS       ##CRAC            1            CR Auto Complete
23   C                   PARM      *BLANKS       ##PREO            1            CR Auto Complete
24   C                   PARM      *BLANKS       ##PARN            1            Par/Non-Par/Both
25   C                   PARM      *BLANKS       ##MGRI            1            Multi Group Restr
26   C                   PARM      *BLANKS       ##KCIF            1            Keep Claimant info
27   C                   PARM      *BLANKS       ##CURS            1            PBILLS entry cursor
28   C                   PARM      *BLANKS       ##PCIB            1            Prov Check in Batch
29   C                   PARM      *BLANKS       ##CLT#            3            Client Number
30   C                   PARM      *BLANKS       ##UCPF            1            UCR Percentile
31   C                   PARM      *BLANKS       ##DCF             1            Data Center
32   C                   PARM      *BLANKS       ##AUTB            1            Auto Batch   Flag
33   C                   PARM      *BLANKS       ##DEIF            1            DE/Image Flag
     C*** END   ADD *** 06/14/04 **************************************
     C*
     C                   MOVEL     '*STMF   '    @@TYPE           12
     C                   MOVEL     'OI'          @@WSID            2            DFLT WORK STN ID
     **** ADD 08/30/05 ***************************************************
      *
      *** RETRIEVE THE DATAAREA THAT HOLDS THE WSID USED FOR OBJIMPT
     C                   IN        OBJIMP                               12
      *** IF THE VALUE EXISTS IN THE DATAAREA, USE IT, ELSE LEAVE THE DFT VALUE
     C     X#WSID        IFNE      *BLANKS
     C     *IN12         ANDEQ     *OFF
     C                   MOVEL     X#WSID        @@WSID
     C                   ENDIF
      *
     **** END ADD 08/30/05 ***************************************************
     C*
     C                   TIME                    DSDTTM
      *** Beg add ***** 11/02/10 ***********************************************
     C                   CALL      RPGM
     C                   PARM                    ROOT             10
      *** End add ***** 11/02/10 ***********************************************
     C*
      *
     C     @@RRN         CHAIN     PI101                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '01'          RTNCD
     C                   GOTO      ENDIT
     C                   ENDIF
     C*
     C*** ADD 6/13/06 ********************************************
     C     REDO1         TAG
     C*** END ADD 6/13/06 ****************************************
     C     FLKY          CHAIN     PFL01                              61
     C     *IN61         IFEQ      *ON
     C*** ADD 6/13/06 ********************************************
     C     @@TYPE        IFEQ      '*STMF   '
     C                   GOTO      REDO2
     C                   ENDIF
     C*** END ADD 6/13/06 ****************************************
     C                   MOVE      '02'          RTNCD
     C                   GOTO      ENDIT
     C                   ENDIF
      *
     C     *IN61         DOWEQ     *OFF
      *
      ***   OPEN PRINT FILES
      *
     C     XXPRT1        IFNE      '1'
     C                   OPEN      QSYSPRT1
     C                   MOVE      '1'           XXPRT1            1
     C                   EXCEPT    HEADE
     C                   ENDIF
      *
      *** BEG DEL *** 06/19/06 ****************************
     C********** XXPRT2    IFNE '1'
     C**********           OPEN QSYSPRT2
     C**********           MOVE '1'       XXPRT2  1
     C**********           EXCPTHEADS
     C**********           ENDIF
      *** END DEL *** 06/19/06 ****************************
      *
      *** CLEAR WORK FIELDS
      *
     C                   MOVEA     *BLANKS       R3
     C                   MOVEA     *BLANKS       R2
     C                   MOVEA     *ZEROS        FND
     C                   MOVEL     *BLANKS       @@FULL
     C                   MOVEL     *BLANKS       @@EXTN
     C                   MOVEL     *BLANKS       @@DICD
     C                   MOVEL     *BLANKS       @@ERCD
     C                   MOVEL     *BLANKS       @@DLCD
     C                   MOVEL     *BLANKS       @@ARCD
     C                   MOVEL     *BLANKS       @@QDCD
     C                   MOVEL     *BLANKS       @@FBCD
     C                   MOVEL     *BLANKS       @@VRCD
     C                   MOVE      *BLANKS       @@TDIR
     C                   MOVE      *BLANKS       XXTDIR
     C                   MOVE      *ZEROS        E1
     C                   MOVE      *ZEROS        E2               30
     C                   MOVE      *ZEROS        E3               30
     C                   MOVE      *ZEROS        E4               30
     C                   MOVE      *ZEROS        S2               30
     C                   MOVE      *ZEROS        S3               30
     C                   MOVE      *ZEROS        S4               30
     C                   MOVE      *ZEROS        L2               30
     C                   MOVE      *ZEROS        L3               30
     C                   MOVE      *ZEROS        L4                3 0
     C                   MOVE      *ZEROS        XT
     C                   MOVEA     '00000000'    *IN(90)
     C                   MOVEA     '00'          *IN(98)
     C                   MOVEL     *BLANKS       XXHKEY
     C**************       MOVEL*BLANKS   XXHKY1
     C                   MOVEL     *BLANKS       NWKEY
     C**************       MOVEL*BLANKS   NWKY1
     C                   MOVEL     *BLANKS       XXHDT             8
     C                   MOVEL     *BLANKS       XXHTM             6
     C                   MOVEL     *BLANKS       XXHDSC          132
     C*** QUOTE VALUE ******************************************
     C                   MOVE      'Q'           #@Q
     C                   MOVE      'Q'           #$Q
      *
     C*** ADD 6/14/06 ***********************************
      *** ADDTIONAL VARIABLES
     C                   MOVEL     *BLANKS       @3FULL
     C                   MOVEL     *BLANKS       INRD              8
     C                   MOVEL     *BLANKS       INDS             40
     C                   MOVEL     *BLANKS       INDD              8
     C                   MOVEL     *BLANKS       INRC              5
     C                   MOVEL     *BLANKS       INDU             10
     C                   MOVEL     *BLANKS       @@TXCD            2
     C                   MOVEL     *BLANKS       @@OPCD            2
     C                   MOVEL     *BLANKS       @@IPCD            2
     C                   MOVEL     *BLANKS       @@ADCD            2
     C                   MOVEL     *BLANKS       @@NDCD            2
     C                   MOVEL     *BLANKS       @@A2CD            2
      *
      *
     C                   EXSR      EXTDRC                                         EXTRACT DIRECTIVES
      *                                                      FROM THE FILE NAME
      *
     *** EXTENSION WILL DETERMINE IF WE DO SPECIAL PROCESSING FOR A TEXT FILE
     C     LO:HI         XLATE(P)  @@EXTN        @@EXTN
     C     @@EXTN        IFEQ      '.IDX'
     C*** THIS IS A SPECIAL FILE THAT NEEDS ADDTIONAL PROCESSING
     C                   EXSR      TXTIMP                                          TEXT IMP. FILE
     C*                                                       DONE WITH THIS FIL
     C                   GOTO      FLTAG
     C**
     C                   ENDIF
     C*
      *
     C*** END ADD 6/14/06 ***********************************
      *
     C                   EXSR      PRCFIL
      *
     C*** ADD 6/14/06 ******************
     C     FLTAG         TAG
     C*** END ADD 6/14/06 ******************
      ***   LOOP TO NEXT SXFL RECORD
     C     FLKY          READE     PFL01                                  61
      *
     C                   ENDDO
      *
     C*** ADD 6/13/06 ********************************************
     C     @@TYPE        IFEQ      '*STMF   '
     C     REDO2         TAG
     C                   MOVEL     '*DSTMF  '    @@TYPE
     C                   GOTO      REDO1
     C                   ENDIF
     C*** END ADD 6/13/06 ****************************************
     C*
      *
      * End the program
     C     ENDIT         TAG
      *** BEG ADD *** 06/19/06 ****************************
     C                   MOVE      'Y'           QQINLR
     C                   EXSR      WRTLOG
      *** END ADD *** 06/19/06 ****************************
     C                   MOVE      *ON           *INLR
      *
      *===================* END OF MAINLINE CODE *====================*
      *
      *****************************************************************
      * * *     SUBROUTINE - PRCFIL - PROCESS FILES FROM IFS      * * *
      *****************************************************************
      *
     C     PRCFIL        BEGSR
      *
      *
      ***   FOR EACH RCD,
      ***     SUBROUTINE TO EXTRACT KEY VALUE FROM FILE NAME
      ***     EXTRACT KEY
      *
      *** CHG 6/14/06 *****************************************
     ***** THIS IS DONE IN THE EXTDRC SUB NOW ***
     C  N99
     CAN 99              EXSR      NAMFLD                                       DONT EXECUTE
     C*** DEL ***          EXSR NAMFLD                        SEE EXTDRC
      *** END CHG 6/14/06 *****************************************
      *
      *** TAKE DATA FOUND, AND SETUP VARIABLES
      *
     C                   MOVE      *BLANKS       #$CLR
     C******************** M\VE *BLANKS   #$CLR1
     C     ' '           CHECKR    XXHKEY        XXHLKN            3 0            FIND LEN OF KEY
      ***
     C                   MOVE      *BLANKS       @@KEY
     C********** XXHLKN    IFGT 9
     C*** INC. YEAR IN KEY
     C                   MOVE      #$CO#         #@CO#
     C                   MOVE      #$FND         #@FND
     C                   MOVE      #$EMP#        #@EMP#
     C                   MOVE      #$DIV         #@DIV
     C                   MOVEL     NWKEY         @@KEY            40
     C******************** ENDIF
     C*******************  MOVELNWKEY     @@KEY  40
     C*****************    MOVELXXHKEY    @@KEY  40
      *
      *** CONVERT DATE/TIME TO USEFUL VALUES
      *
      * DATE TO MMDDCCYY
     C*
     C                   MOVE      *ZEROS        SRDTI
     C************         MOVE XXHDT     SRDTI
     C                   MOVE      DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         @@CDAT            8            DT FOR API
      *
      * TIME AS IS 6A
     C                   MOVE      *ZEROS        @@CTIM
     C***********          MOVELXXHTM     @@CTIM  6
     C                   MOVEL     DSTIME        @@CTIM            6
      *
      * DESC AS IS 132 (AS COMMENT IN DARTS, OR MOREOBJECTS)
      *
     C                   MOVE      *BLANKS       @@DESC
     C                   MOVE      *BLANKS       @@DESC          132
     C*******************  MOVELXXHDSC    @@DESC132
      *
      ***        VALIDATE KEY IN SAME SUBROUTINE
      ***  VALIDATE KEY
     C                   CALL      'ISIV05R'                                    COULD GET FRM FIL
     C                   PARM                    @@KEY            40
     C                   PARM                    @@RTNC            1
      *
      ***        NOT VALID
     C     @@RTNC        IFNE      ' '
     C                   MOVEL     @@RTNC        @@VRCD            2
      *
      *** EXECUTE ERROR SUBROUTINE ***
      *
     C                   EXSR      ERROR1
      *
      * @@ERCD-COPY TO ERROR DIRECTORY, @@DLCD-DELETE FROM ORIGINAL DIRECTRY
      *
      *** FORMAT THE ERROR REPORT DETAIL LINE HERE ***
      *
      *
     C****       *INOA     IFEQ *ON
     C*                    EXCPTHEADE
     C*                    ENDIF
      *
     C*                    EXCPTERR1
      *
     C*** LOOP FOR THE ERRORS
     C*          *INOA     IFEQ *ON
     C*                    EXCPTHEADE
     C*                    EXCPTERR1
     C*                    ENDIF
      *
     C****                 MOVEL*BLANKS   $$ERCD
     C*                    MOVEL*BLANKS   $$ERDS
     C*                    MOVEL'VR'      $$ERCD  2
     C*                    MOVEL@VR       $$ERDS 80
     C****                 EXCPTERR2
      *
     C*          @@ERCD    IFNE *BLANK
     C*                    MOVEL*BLANKS   $$ERCD
     C*                    MOVEL*BLANKS   $$ERDS
     C*                    MOVEL@@ERCD    $$ERCD  2
     C*                    MOVEL@ER       $$ERDS 80
     C*                    EXCPTERR2
     C***                  ENDIF
     C*
      *
     C*          @@DLCD    IFNE *BLANK
     C*                    MOVEL*BLANKS   $$ERCD
     C*                    MOVEL*BLANKS   $$ERDS
     C*                    MOVEL@@DLCD    $$ERCD  2
     C*                    MOVEL@DL       $$ERDS 80
     C*                    EXCPTERR2
     C*                    ENDIF
     C*
      *
     C                   ELSE
      ***        VALID -
      ***             GET NEW NAME FOR THE FILE (QDLS/FOLDER BALANCE)
      *
     C                   CALL      'SCI107R'
     C                   PARM                    @@WSID
     C                   PARM      '000000'      @@DSEQ            6
     C                   PARM      *BLANKS       @@DSFX            3
     C                   PARM      '0000'        @@BSEQ            4
     C                   PARM      *BLANKS       @@IDOC           12
     C                   PARM      *BLANKS       @@IBCH           12
     C                   PARM      *BLANKS       @@IFLR            8
     C                   PARM      *BLANKS       @@ISFL           12
     C                   PARM                    @@EXTN            4
     C                   PARM      ' '           @@RTN3            1
      *
      *** IF RTN = 1, DOC INDEX RECORD NO FOUND FOR THIS WSID (OI)
      *
      *
     C     @@RTN3        IFNE      *BLANKS
     C                   MOVEL     @@RTN3        @@DICD            1
     C                   ENDIF
      *
      ***     ONLY PROCESS IF QDLS INFO IS RETRIEVED
     C     @@RTN3        IFEQ      *BLANK
      *
      ***             COPY OBJECT FROM THE IFS FOLDER TO THE NEW QDLS FOLDER
      *
      ***    BUILD TOFLR/OBJ INFO FOR CPYOBJ API CALL
     C*** BEGIN DEL *** 06/21/04 **************************************
     C**********           MOVE *BLANKS   @@WK1
     C**********           MOVE *BLANKS   @@WK2
     C*** END   DEL *** 06/21/04 **************************************
     C                   MOVE      *BLANKS       @@WK3
     C*** BEGIN DEL *** 06/21/04 **************************************
     C********** '\QDLS\'  CAT  @@IFLR:0  @@WK1  14
     C********** @@WK1     CAT  '\':0     @@WK2  15
     C********** @@WK2     CAT  @@ISFL:0  @@WK3  27
     C*** END   DEL *** 06/21/04 **************************************
     C*** BEGIN ADD *** 06/21/04 **************************************
     C                   CALL      'SA030R'
     C                   PARM      *BLANKS       ##SUFX            3
     C                   PARM      *BLANKS       ##PFLR           80
     C*
     C     ##PFLR        IFNE      *BLANKS
      *** Beg chg ***** 11/02/10 ***********************************************
     C*******    '\QDLS\'  CAT  ##PFLR:0  @@WK3  80 P
     C     '\'           CAT(P)    ROOT          RPATH            12
     C     RPATH         CAT(P)    '\':0         RPATH
     C     RPATH         CAT(P)    ##PFLR:0      @@WK3            80
      *** End chg ***** 11/02/10 ***********************************************
     C                   CAT       '\':0         @@WK3
     C                   CAT       @@IFLR:0      @@WK3
     C                   CAT       '\':0         @@WK3
     C                   CAT       @@ISFL:0      @@WK3
     C                   ELSE
      *** Beg chg ***** 11/02/10 ***********************************************
     C********   '\QDLS\'  CAT  @@IFLR:0  @@WK3  80 P
     C     '\'           CAT(P)    ROOT          RPATH
     C     RPATH         CAT(P)    '\':0         RPATH
     C     RPATH         CAT(P)    @@IFLR:0      @@WK3            80
      *** End chg ***** 11/02/10 ***********************************************
     C                   CAT       '\':0         @@WK3
     C                   CAT       @@ISFL:0      @@WK3
     C                   ENDIF
     C*** END   ADD *** 06/21/04 **************************************
     C*
      *
      *
     C                   CALL      'CPYOBJC'
     C                   PARM      I1IDIR        @@FDIR           80
     C                   PARM      FLOBJN        @@FOBJ          132
     C                   PARM      @@WK3         @@TDIR           80
     C                   PARM      @@IDOC        @@TOBJ          132
     C                   PARM      '  '          @@RTN2            2
     C*
     C     '\'           CAT(P)    @@TOBJ:0      WK133           133
     C     @@TDIR        CAT(P)    WK133:0       WK212           212
     C                   MOVEL     WK212         XXTDIR           60
      *
      *  ** IF RTN CODE  THEN PRINT ON REPORT, NO SUCCESSFUL COPY TO QDLS
      *
     C     @@RTN2        IFNE      *BLANKS
     C                   MOVEL     @@RTN2        @@QDCD            2
     C                   ELSE
     C                   MOVEL     *BLANKS       @@QDCD
     C                   ENDIF
      *
     C     @@QDCD        IFEQ      *BLANKS
      *
      ***             CALL PGM TO UPDATE THE DI AND FB RCDS.. NEXT AVAIL
      *
     C                   CALL      'SCI106R'
     C                   PARM                    @@WSID
     C                   PARM                    @@DSEQ            6
     C                   PARM                    @@BSEQ            4
     C                   PARM                    @@IFLR
     C                   PARM      ' '           @@RTN4            1
      *
     C     @@RTN4        IFNE      *BLANKS
     C                   MOVEL     @@RTN4        @@FBCD            2
     C                   ELSE
     C                   MOVEL     *BLANKS       @@FBCD
     C                   ENDIF
      *
     C     @@FBCD        IFEQ      *BLANKS
      *
      ***             WRITE A DART &/OR MORE OBJECT RECORD
     C*** DETERMINE WHICH TYPE OF RECORD WE ARE WRITING...
     C*
     C     I1IDXT        CASEQ     'D'           DRTRCD                         IF MAJOR DART
     C     I1IDXT        CASEQ     'M'           MORCD                          IF MORE OBJ RCD
     C                   CAS                     MORCD                          ANY OTHER
     C                   ENDCS
      ***             MOVE OBJECT TO THE ARCHIVE FOLDER  (IFS)
      *
     C     I1ADIR        IFNE      *BLANKS
      *
     C*** ADD 02/27/03 *** C:02 *************************
     C                   TIME                    DSDTTM
     C                   MOVE      DSDATE        $$DAT             6
     C                   MOVE      DSTIME        $$TIM             6
     C                   MOVEL     $$DAT         $$DTTM           12
     C                   MOVE      $$TIM         $$DTTM
     C*
     C     $$DTTM        CAT(P)    FLOBJN:0      $$NOBJ          132
     C*
      *
     C***  END ADD ***8 02/27/03 ********8
      *
     C                   CALL      'CPYOBJC'
     C                   PARM      I1IDIR        @@FDIR           80
     C                   PARM      FLOBJN        @@FOBJ          132
     C                   PARM      I1ADIR        @@TDIR           80
     C**** CHG 02/27/03 ** PARM FLOBJN    @@TOBJ132
     C                   PARM      $$NOBJ        @@TOBJ          132
     C                   PARM      '  '          @@RTN2            2
      *
      *  ** IF RTN CODE  THEN PRINT ON REPORT, NO ARCHIVE!!!
      *
     C     @@RTN2        IFNE      *BLANKS
     C                   MOVEL     @@RTN2        @@ARCD            2
     C                   ELSE
     C                   MOVEL     *BLANKS       @@ARCD
     C                   ENDIF
      *
     C                   ENDIF                                                   IF I1ADIR
      *
     C     @@ARCD        IFEQ      *BLANKS
      *
      *** ONLY DELETE IF THE COPY ABOVE WAS SUCCESSFUL (TO ERROR DIRECTORY)
      *
     C                   CALL      'DELOBJC'
     C                   PARM      I1IDIR        @@FDIR           80
     C                   PARM      FLOBJN        @@FOBJ          132
     C                   PARM      '  '          @@RTN2            2
      *
     C     @@RTN2        IFNE      *BLANKS
     C                   MOVEL     @@RTN2        @@DLCD            2
     C                   ELSE
     C                   MOVEL     *BLANKS       @@DLCD
     C                   ENDIF
      *
     C                   ENDIF                                                  @@ARCD
      *
      *
     C                   ENDIF                                                  IF FB OK  (FBCD)
     C                   ENDIF                                                  IF QDLS OK (QDCD)
      ***
     C                   ELSE
     C*** ERROR ON DI RECORD... COULDN'T CONTINUE... ERROR RPT???
      *
     C                   ENDIF
     C                   ENDIF
     C*** Error or Success Report here
      *
     C     @@DICD        IFNE      *BLANK
     C     @@ERCD        ORNE      *BLANK
     C     @@QDCD        ORNE      *BLANK
     C     @@FBCD        ORNE      *BLANK
     C     @@ARCD        ORNE      *BLANK
     C     @@DLCD        ORNE      *BLANK
     C     @@VRCD        ORNE      *BLANK
      ***   ERROR REPORT
      *
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEADE
     C                   ENDIF
      *
     C                   EXCEPT    ERR1
      *
     C*** LOOP FOR THE ERRORS
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEADE
     C                   EXCEPT    ERR1
     C                   ENDIF
      *
     C     @@DICD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@DICD        $$ERCD            2
     C*** BEGIN ADD *** 05/30/08 **************************************
     C     @@DICD        IFEQ      'S'
     C*** BEGIN ADD *** 01/28/10 **************************************
     C                   CALL      'RTVBRND'                            99
     C                   PARM      'D'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C*
     C                   MOVEL     #BRND1        @RO              80
     C*** END ADD  **** 01/28/10 **************************************
     C                   MOVEL     @RO           $$ERDS
     C                   ELSE
     C*** END   ADD *** 05/30/08 **************************************
     C                   MOVEL     @DI           $$ERDS           80
     C*** BEGIN ADD *** 05/30/08 **************************************
     C                   ENDIF
     C*** END   ADD *** 05/30/08 **************************************
      *** BEG ADD *** 06/19/06 ****************************
     C                   MOVE      'U'           QQSTS
     C                   EXSR      WRTLOG
      *** END ADD *** 06/19/06 ****************************
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
     C*
     C     @@ERCD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@ERCD        $$ERCD            2
     C                   MOVEL     @ER           $$ERDS           80
      *** BEG ADD *** 06/19/06 ****************************
     C                   MOVE      'U'           QQSTS
     C                   EXSR      WRTLOG
      *** END ADD *** 06/19/06 ****************************
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
     C     @@QDCD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@QDCD        $$ERCD            2
     C                   MOVEL     @QD           $$ERDS           80
      *** BEG ADD *** 06/19/06 ****************************
     C                   MOVE      'U'           QQSTS
     C                   EXSR      WRTLOG
      *** END ADD *** 06/19/06 ****************************
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
     C     @@FBCD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@FBCD        $$ERCD            2
     C*** BEGIN ADD *** 05/30/08 **************************************
     C     @@FBCD        IFEQ      'S'
     C*** BEGIN ADD *** 01/28/10 **************************************
     C                   CALL      'RTVBRND'                            99
     C                   PARM      'D'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C*
     C                   MOVEL     #BRND1        @RO              80
     C*** END ADD  **** 01/28/10 **************************************
     C                   MOVEL     @RO           $$ERDS
     C                   ELSE
     C*** END   ADD *** 05/30/08 **************************************
     C                   MOVEL     @FB           $$ERDS           80
     C*** BEGIN ADD *** 05/30/08 **************************************
     C                   ENDIF
     C*** END   ADD *** 05/30/08 **************************************
      *** BEG ADD *** 06/19/06 ****************************
     C                   MOVE      'U'           QQSTS
     C                   EXSR      WRTLOG
      *** END ADD *** 06/19/06 ****************************
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
      *
     C*
     C     @@ARCD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@ARCD        $$ERCD            2
     C                   MOVEL     @AR           $$ERDS           80
      *** BEG ADD *** 06/19/06 ****************************
     C                   MOVE      'U'           QQSTS
     C                   EXSR      WRTLOG
      *** END ADD *** 06/19/06 ****************************
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
     C*
     C     @@DLCD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@DLCD        $$ERCD            2
     C                   MOVEL     @DL           $$ERDS           80
      *** BEG ADD *** 06/19/06 ****************************
     C                   MOVE      'U'           QQSTS
     C                   EXSR      WRTLOG
      *** END ADD *** 06/19/06 ****************************
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
     C*
     C     @@VRCD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@VRCD        $$ERCD            2
     C                   MOVEL     @VR           $$ERDS           80
      *** BEG ADD *** 06/19/06 ****************************
     C                   MOVE      'U'           QQSTS
     C                   EXSR      WRTLOG
      *** END ADD *** 06/19/06 ****************************
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
     C                   EXCEPT    ERR3
     C*** Begin Add *** 06/25/20 *******************************************
     C                   ExSr      SndEmail
     C*** End   Add *** 06/25/20 *******************************************
      *
     C                   ELSE
      *
      *
      ***   WRITE SUCCESS REPORT
      *** BEG DEL *** 06/19/06 ****************************
     C*********            EXCPTSUCCS1
      *** END DEL *** 06/19/06 ****************************
      *** BEG ADD *** 06/19/06 ****************************
     C                   MOVE      'S'           QQSTS
     C                   EXSR      WRTLOG
      *** END ADD *** 06/19/06 ****************************
     C                   ENDIF
     C                   ENDSR
      *
      *
      *****************************************************************
      * * *     SUBROUTINE - NAMFLD - BREAK FILE NAME TO FIELDS   * * *
      *****************************************************************
      *
     C     NAMFLD        BEGSR
      *
      *** CLEAR WORK FIELDS
      *
     C                   MOVEL     FLOBJN        @@FULL          132
      *
      *** EXTRACT THE KEY, DATE, TIME, DESCRIPTION FROM THE FULL OBJ NAME
      *                    FND IS AN ARRAY AND CONTAINS ALL OCCURANCES OF
      *                    " "
      *
     C     ' '           SCAN      @@FULL:1      FND                      90
     C     '.'           SCAN      @@FULL:1      XT                3 0    99      FILE EXTN STRT
     C*** GET EXTENTION TO FILE
     C     4             SUBST     @@FULL:XT     @@EXTN            4              FILE EXTNTN
     C                   MOVEL     @@EXTN        WK1               1
     C     WK1           IFNE      '.'
     C                   MOVEL     '.'           @@EXTN
     C                   MOVE      'OBJ'         @@EXTN
     C                   ENDIF
     C*
     C     FND(1)        SUB       1             E1                3 0
     C     E1            IFGT      0
      *  EXTRACT THE KEY
     C     E1            SUBST     @@FULL:1      XXHKEY           40    91      HOLD KEY
     C*******    E1        SUBST@@FULL:1  XXHKY1 40    91  HOLD KEY
      *  EXTRACT THE DATE
     C*********  FND,1     ADD  1         S2      30       START OF 2ND PART
     C***        FND,2     SUB  S2        L2      30       LENGTH OF 2ND PRT
     C***        FND,2     SUB  1         E2      30       END OF 2ND PRT
     C*
     C********   L2        SUBST@@FULL:S2 XXHDT   6    92  HOLD DATE
      *
      *  EXTRACT THE TIME
     C********   FND,2     ADD  1         S3      30       START OF 3RD PART
     C**         FND,3     SUB  S3        L3      30       LENGTH OF 3RD PRT
     C**         FND,3     SUB  1         E3      30       END OF 3RD PRT
     C*
     C********   L3        SUBST@@FULL:S3 XXHTM   6    93  HOLD TIME
      *
      *
      *  EXTRACT THE DESCRIPTION
     C     *IN99         IFEQ      *OFF                                         OF @@FULL OR NO
     C************         Z-ADD133       FND,4            EXTNTN FOUND..
     C                   ELSE
     C************         Z-ADDXT        FND,4            ONLY ONE '.'
     C                   ENDIF                                                                   NDI
     C********   FND,3     ADD  1         S4      30       START OF 3RD PART
     C********   FND,4     SUB  S4        L4      30       LENGTH OF 3RD PRT
     C********   FND,4     SUB  1         E4      30       END OF 3RD PRT
     C*
     C********   L4        SUBST@@FULL:S4 XXHDSC132    94  HOLD DESCRIPTION
      *
      *
     C                   ENDIF
      *
      *** SUBSTRING ERROR
      ***  IF 91,2,3, OR 4 IS ON, THEN AN ERROR OCCURED ON A SUBSTR... NO
      ***  VALUE WAS CAPTURED
      *
     C                   ENDSR
      *
      *
      *****************************************************************
      * * *     SUBROUTINE - ERROR1 - ERROR SUBROUTINE 1          * * *
      *****************************************************************
      *
     C     ERROR1        BEGSR
      *
      ***           MOVE TO ERROR DIRECTORY
      *
     C     I1EDIR        IFNE      *BLANKS
     C*** ADD 02/27/03 *** C:02 *************************
     C                   TIME                    DSDTTM
     C                   MOVE      DSDATE        $$DAT             6
     C                   MOVE      DSTIME        $$TIM             6
     C                   MOVEL     $$DAT         $$DTTM           12
     C                   MOVE      $$TIM         $$DTTM
     C*
     C     $$DTTM        CAT(P)    FLOBJN:0      $$NOBJ          132
     C*
      *
     C***  END ADD ***8 02/27/03 ********8
      *
     C                   CALL      'CPYOBJC'
     C                   PARM      I1IDIR        @@FDIR           80
     C                   PARM      FLOBJN        @@FOBJ          132
     C                   PARM      I1EDIR        @@TDIR           80
     C**** CHG 02/27/03 ** PARM FLOBJN    @@TOBJ132
     C                   PARM      $$NOBJ        @@TOBJ          132
     C                   PARM      '  '          @@RTN2            2
      *
      *** IF RTNCD, PRINT ON ERROR REPORT ALSO
      *
     C     @@RTN2        IFNE      *BLANKS
     C                   MOVEL     @@RTN2        @@ERCD            2
     C                   ELSE
     C                   MOVEL     *BLANKS       @@ERCD
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     @@ERCD        IFEQ      *BLANKS
      *
      *** ONLY DELETE IF THE COPY ABOVE WAS SUCCESSFUL (TO ERROR DIRECTORY)
      *
     C                   CALL      'DELOBJC'
     C                   PARM      I1IDIR        @@FDIR           80
     C                   PARM      FLOBJN        @@FOBJ          132
     C                   PARM      '  '          @@RTN2            2
      *
     C     @@RTN2        IFNE      *BLANKS
     C                   MOVEL     @@RTN2        @@DLCD            2
     C                   ELSE
     C                   MOVEL     *BLANKS       @@DLCD
     C                   ENDIF
      *
     C                   ENDIF
     C                   ENDSR
      *
      *****************************************************************
      * * *     SUBROUTINE - DRTRCD - CREATE DART RECORD FOR IMPT * * *
      *****************************************************************
      *
     C     DRTRCD        BEGSR
      *
     C*** WRITE REMARK RECORD
     C*
     C*  *** PROGRAM NAME TO USE FOR PNAM - KEY STRUCTURE
     C                   MOVEL     'W0050R  '    ##PNAM           10
     C*          ------------------------------------------------------
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  @@KEY         Q#KEY            40
     C*** ADD 6/14/06 ***************************************
     *** REMARK CODE INFORMATION HERE ***
     C     INRC          IFNE      *BLANKS
     C                   MOVEL(P)  INRC          XXXRC            10
     **
     C                   CALL      'DA200R'
     C                   PARM      XXXRC         #@RMCD            5
     C                   PARM      *BLANKS       #@DSC            40
     C                   PARM      *BLANK        #@RTN             1
      *
     C     #@RTN         IFEQ      'Y'
     C                   MOVEL(P)  INRC          NEWRC             5
     C                   ELSE
     C                   MOVEL     I1RMCD        NEWRC
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     I1RMCD        NEWRC
     C                   ENDIF
     C*** END ADD 6/14/06 ***************************************
     C*** DETERMINE THE COMMENT FIELD
     C                   MOVEL     I1DFTT        XXWK5             5
     C     XXWK5         IFEQ      '*FILE'
     ** ADD 6/14/06 ***********************************
     C     INDS          IFNE      *BLANKS
     C     INDS          CAT(P)    FLOBJN:3      WK175A          175
     C                   MOVEL     *BLANKS       Q#COMM
     **                    MOVELWK175A    Q#COMM    P
     C                   ELSE
     C                   MOVEL     *BLANKS       Q#COMM
     C                   MOVEL(P)  FLOBJN        Q#COMM          256
     C                   ENDIF
     C*** END ADD 6/14/06 ***************************************
     C*** USE ONLY FILE NAME FOR REMARK TEXT
     C*** CHG 6/14/06 ********************************
     C*** RMV AS CHG ***   MOVELFLOBJN    Q#COMM256 P
     C*** END CHG 6/14/06 ********************************
     C                   ELSE
     C     I1DFTT        IFEQ      *BLANKS
      *** PUT IN RMCD TEXT
     C                   CALL      'DA200R'
     C*** CHG 6/14/06 ********************************
     C******************   PARM I1RMCD    #@RMCD  5
     C                   PARM      NEWRC         #@RMCD            5
     C*** END CHG 6/14/06 ********************************
     C                   PARM      *BLANKS       #@DSC            40
     C                   PARM      *BLANK        #@RTN             1
      *
     C     #@RTN         IFEQ      'Y'
     C                   MOVEL(P)  #@DSC         Q#COMM
     C                   ELSE
     C                   MOVEL(P)  FLOBJN        Q#COMM          256
     C                   ENDIF
     ** ADD 6/14/06 ***********************************
     C     INDS          IFNE      *BLANKS
     C                   MOVEL     *BLANKS       Q#COMM
     C                   MOVEL     INDS          Q#COMM
     C                   ENDIF
     C*** END ADD 6/14/06 ***************************************
     C                   ELSE
     C*** USE VALUE *CAT FILENAME
     C     I1DFTT        CAT(P)    FLOBJN:3      WK175           175
     C                   MOVEL(P)  WK175         Q#COMM
     ** ADD 6/14/06 ***********************************
     C     INDS          IFNE      *BLANKS
     C                   MOVEL     *BLANKS       Q#COMM
     C                   MOVEL     INDS          Q#COMM
     C                   ENDIF
     C*** END ADD 6/14/06 ***************************************
     C                   ENDIF                                                  IF = *BLANKS
     C                   ENDIF                                                  IF = *FILE
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   MOVEL(P)  @@CDAT        Q#CDAT            8
     ** ADD 6/14/06 ***********************************
     C     INRD          IFNE      *BLANKS
     C* VALIDATE THE DATE, THEN MOVE TO Q#CDAT (AND @@CDAT)
     C                   MOVE      *ZEROS        SRDTI
     C                   MOVE      INRD          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C     SRERR         IFEQ      '0'
     C                   MOVEL     SRDTO         @@CDAT            8            DT FOR API
     C                   MOVE      INRD          Q#CDAT
     C                   ENDIF
     C                   ENDIF
     C*** END ADD 6/14/06 ***************************************
     ** ADD 6/14/06 ***********************************
     C     INDD          IFNE      *BLANKS
     C* VALIDATE THE DATE, THEN MOVE TO Q#CDAT (AND @@CDAT) DIARY DATE OVER
     C*** RIDES THE RD IF ENTERED.
     C                   MOVE      *ZEROS        SRDTI
     C                   MOVE      INDD          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C     SRERR         IFEQ      '0'
     C                   MOVEL     SRDTO         @@CDAT            8            DT FOR API
     C                   MOVE      INDD          Q#CDAT
     C                   ENDIF
     C                   ENDIF
     C*** END ADD 6/14/06 ***************************************
     C     I1IDXT        IFEQ      'M'
     C                   MOVEL     *BLANKS       Q#DCID
     C                   MOVEL     *BLANKS       Q#IMGF
     C                   ELSE
     C                   MOVEL(P)  I1IMGF        Q#IMGF            1
     C                   MOVEL(P)  @@IDOC        Q#DCID           12
     C                   ENDIF
     C                   MOVEL(P)  ##PNAM        Q#PNAM           10
     C*** COULD HAVE SECURITY VALUE FROM SMI1P?????
     C                   MOVE      *ZEROS        ##SECL            3
     C*** SECURE BY LEVEL = 'N' BECAUSE WE CANT SECURE WITHOUT SMI1P FILE
     C                   MOVEL(P)  'N'           Q#SBYL            1
      *
     C                   MOVE      '000'         Q#SECL            3
     C                   MOVE      *BLANKS       SCDUSR           10
     C*** COULD HAVE DIARY USER IN THE SMI1P FILE
     C     SCDUSR        IFNE      *BLANKS
     C                   MOVEL(P)  SCDUSR        Q#USER           10
     C                   ELSE                                                   DUSR = blanks
     C                   MOVEL(P)  Q1USER        Q#USER           10
     C                   ENDIF                                                  DUSR = blanks
     C*** ADD 6/14/06 ***************************************
     *** DIARY USER INFORMATION HERE ***
     C     INDU          IFNE      *BLANKS
     C                   MOVEL(P)  INDU          Q#USER
     C                   ENDIF
     C*** END ADD 6/14/06 ***************************************
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C     @@IFLR        CAT       '/':0         WK9               9
     C     WK9           CAT       @@ISFL:0      WK21             21
     C                   MOVEL(P)  WK21          Q#FOLD           80
     C                   MOVE      *BLANKS       SCDITP            1
     C*** IF DIARY USER ENTERED IN THE SMI1P FILE, DIARY WOULD = 'Y'
     C     SCDITP        IFEQ      'Y'
     C     SCDITP        OREQ      'N'
     C                   MOVE      SCDITP        Q#DIFL            1
     C                   ELSE                                                   DITP = 'Y'
     C                   MOVE      *BLANK        Q#DIFL            1
     C                   ENDIF                                                  DITP = 'Y'
     C*** ADD 6/14/06 ***************************************
     *** DIARY USER INFORMATION HERE ***
     C     INDU          IFNE      *BLANKS
     C                   MOVEL     'Y'           Q#DIFL
     C                   ENDIF
     C*** END ADD 6/14/06 ***************************************
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C*** BEGIN DEL *** 06/14/04 **************************************
     C**********           MOVEL*BLANK    Q#FL01  1 P
     C*** END   DEL *** 06/14/04 **************************************
     C                   MOVEL(P)  'N'           Q#FL02            1            PURGABLE
     C**** CHG 6/14/06 *************************************
     C                   MOVEL(P)  NEWRC         Q#RMCD            5
     C******************   MOVELI1RMCD    Q#RMCD  5 P
     C**** END CHG 6/14/06 *************************************
     C                   MOVE      '00000'       Q#SEQ#            5
     C*** APPLICATION ID?? WHAT???
     C                   MOVEL(P)  'WC'          Q#APID            2
      *
     C     I1IMGF        IFEQ      'I'
     C                   MOVEL(P)  @@IDOC        Q#IDOC           12
     C                   ENDIF
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C*
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *BLANKS       Q#PIDX            5
     C                   MOVEL(P)  *BLANKS       Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C*** BEGIN ADD *** 06/14/04 **************************************
     C     ##DEIF        IFEQ      'Y'
     C     CTKY          CHAIN     PCT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  CTDEPG        Q#DPGM
     C     CTDEPG        IFNE      *BLANKS
     C                   MOVEL(P)  Q#KEY         Q#DKEY
     C                   MOVEL(P)  'Y'           Q#FL01            1
     C                   ELSE
     C                   CLEAR                   Q#DKEY
     C                   CLEAR                   Q#FL01
     C                   ENDIF
     C                   ELSE
     C                   CLEAR                   Q#DPGM
     C                   CLEAR                   Q#DKEY
     C                   CLEAR                   Q#FL01
     C                   ENDIF
     C                   ELSE
     C                   CLEAR                   Q#DPGM
     C                   CLEAR                   Q#DKEY
     C                   CLEAR                   Q#FL01
     C                   ENDIF
     C*** END   ADD *** 06/14/04 **************************************
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#COMM
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C*** BEGIN ADD *** 06/14/04 **************************************
     C                   PARM      *BLANKS       Q#CSID            2
     C                   PARM      *BLANKS       Q#CODE            2
     C                   PARM      *BLANKS       Q#TRDT            7
     C                   PARM      *BLANKS       Q#TRTM            6
     C                   PARM      *BLANKS       Q#STAT            1
     C                   PARM      *BLANKS       Q#IUSR           10
     C                   PARM      *BLANKS       Q#DCDT            7
     C                   PARM      *BLANKS       Q#FL03            1
     C                   PARM      *BLANKS       Q#FL04            1
     C                   PARM      *BLANKS       Q#FL05            1
     C                   PARM      *BLANKS       Q#IDT             7
     C                   PARM      *BLANKS       Q#VCFN            1
     C                   PARM                    Q#DPGM           10
     C                   PARM                    Q#DKEY           40
     C*** END   ADD *** 06/14/04 **************************************
     C                   MOVEL(P)  @@KEY         QQKEY            40
     C                   MOVE      Q#SEQ#        ##SEQ#            5 0
     C                   MOVEL     *BLANKS       QQRCOD            1
     C*
      *
     C                   ENDSR
      *
      *****************************************************************
      * * *     SUBROUTINE - MORCD - CREATE MORE OBJ RECORD       * * *
      *****************************************************************
      *
     C     MORCD         BEGSR
      *
      *** 1ST CREATE THE MAJOR REMARK RECORD
     C                   EXSR      DRTRCD
      *
     C     I1IMGF        IFEQ      'W'
     C     I1IMGF        OREQ      ' '
     C                   MOVEL     'D'           $$OTYP
     C                   ELSE
     C                   MOVEL     I1IMGF        $$OTYP
     C                   ENDIF
     C*
     C                   CALL      'ISEWMD'
     C                   PARM      @@KEY         $$KEY            40
     C                   PARM      Q#SEQ#        $$SEQ             5
     C*
     C                   PARM      @@IDOC        $$OID            40
     C                   PARM      Q#FOLD        $$OFLR           60
     C                   PARM                    $$OTYP            1
     C                   PARM      FLOBJN        $$ODSC           40
     C                   PARM      Q#RMCD        $$RMCD            5
     C                   PARM      'AS400   '    $$SYSN           20
     C                   PARM      ##PNAM        $$PNAM           10
     C                   PARM      Q1USER        $$USR            10
     C                   PARM      ' '           $$RTN             1
     C                   PARM      *BLANKS       $$ERTN           80
     C*
      *
     C                   ENDSR
      *
     C*** ADD 6/14/06 ***********************************
     C***
     C**====================================================================**
     *** EXTRACT DIRECTIVES FROM THE FILE NAME                              **
     C     EXTDRC        BEGSR
     C**====================================================================**
     C*
     C*** EXTRACT RD_  REMARK DATE,  DU_  DIARY USER,  DD_ DIARY DATE,
     C***  RD_ REMARK DATE, AND DS_ DESCRIPTION FROM THE FILE NAME.
     ***     THE FIRST XX POSITIONS OF THE FILE NAME WILL ALWAYS BE THE
     ***      KEY (POLICY#, CLAIM#, AGENT#, ETC.)
     C*
      *** CLEAR WORK FIELDS
      *
     C                   MOVEL     FLOBJN        @@FULL          132
      *
      *** EXTRACT THE KEY, DATE, TIME, DESCRIPTION FROM THE FULL OBJ NAME
      *                    FND IS AN ARRAY AND CONTAINS ALL OCCURANCES OF
      *                    " "
      *
     C     ' '           SCAN      @@FULL:1      FND                      90       ARRARY WITH
     C     '.'           SCAN      @@FULL:1      XT                3 0    99      FILE EXTN STRT
     C*** GET EXTENTION TO FILE
     C     4             SUBST     @@FULL:XT     @@EXTN            4              FILE EXTNTN
     C                   MOVEL     @@EXTN        WK1               1
     C     WK1           IFNE      '.'
     C                   MOVEL     '.'           @@EXTN
     C                   MOVE      'OBJ'         @@EXTN                           EXTENTION RTVD
     C                   ENDIF
     C*
     C     FND(1)        SUB       1             E1                3 0            END OF KEY
     C     E1            IFGT      0
      *  EXTRACT THE KEY
     C     E1            SUBST     @@FULL:1      XXHKEY           40    91      HOLD KEY
     ***** MOVED FROM HERE TO BELOW 2/12/07 *****************************
     C*
     C                   MOVEL     *BLANKS       @@KEY
     C                   MOVEL     XXHKEY        @@KEY
     **** ADD 2/12/07 **************************************************
     ******* VALIDATE KEY HERE.  IF INVALID, THEN THIS ONE IS ERROR, ELSE
     ******* CONTINUE **************************************
     **
     C                   EXSR      VALKY1
     *** IF ERROR, MUST EXIT HERE ****************************8
     C     @@RTNC        IFNE      *BLANK
     C                   MOVE      *BLANKS       @@RTNC
     C                   GOTO      FLTAG
     C                   ENDIF
     **
     **
     ****** END ADD *** 2/12/07 *****************************************
     ***** MOVED FROM ABOVE 2/12/07 ******************************
     C***  BUILD THE EMPLOYER KEY
     C                   MOVE      #$CO#         @3CO#
     C                   MOVE      #$FND         @3FND
     C                   MOVE      #$EMP#        @3EMP#
     C                   MOVE      #$DIV         @3DIV
     ****  END MOVE 2/12/07 ***************************************
     C**
     ***** CLEAR THE FIELDS BEFORE GETTING THE DATA FROM THE FILE NAME
     **
     ** ++++ ** REMARK DATE **
     C                   MOVEA     *ZEROS        FND
     C                   MOVE      *BLANKS       INRD
     C*
     C     'RD_'         SCAN      @@FULL:1      FND                      90
     C     FND(1)        IFGT      0
     C     FND(1)        ADD       3             RD                3 0
     C     ' '           SCAN      @@FULL:RD     RDEND             3 0
     C     RDEND         IFGT      0
     C     RDEND         SUB       RD            XXAAXX            3 0
     C     XXAAXX        ADD       1             RDLEN             3 0
     C*
     C     RDLEN         IFGT      0
     C     RDLEN         SUBST     @@FULL:RD     INRD
     C                   ELSE
     C                   MOVEL     *BLANKS       INRD
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     *BLANKS       INRD
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     *BLANKS       INRD
     C                   ENDIF
     **
     **    REMARK CODE
     ***=============***
     **
     C                   MOVEA     *ZEROS        FND
     C                   MOVE      *BLANKS       INRC
     C*
     C     'RC_'         SCAN      @@FULL:1      FND                      90
     C     FND(1)        IFGT      0
     C     FND(1)        ADD       3             RC                3 0
     C     ' '           SCAN      @@FULL:RC     RCEND             3 0    90
     C     RCEND         IFGT      0
     C     RCEND         SUB       RC            XXAAXX            3 0
     C     XXAAXX        ADD       1             RCLEN             3 0
     C*
     C     RCLEN         IFGT      0
     C     RCLEN         SUBST     @@FULL:RC     INRC
     C                   ELSE
     C                   MOVEL     *BLANKS       INRC
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     *BLANKS       INRC
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     *BLANKS       INRC
     C                   ENDIF
      *=============================***
     **
     ** ++++ ** DIARY DATE  **
     C                   MOVEA     *ZEROS        FND
     C                   MOVE      *BLANKS       INDD
     C*
     C     'DD_'         SCAN      @@FULL:1      FND                      90
     C     FND(1)        IFGT      0
     C     FND(1)        ADD       3             DD                3 0
     C     ' '           SCAN      @@FULL:DD     DDEND             3 0
     C     DDEND         IFGT      0
     C     DDEND         SUB       DD            XXAAXX            3 0
     C     XXAAXX        ADD       1             DDLEN             3 0
     C*
     C     DDLEN         IFGT      0
     C     DDLEN         SUBST     @@FULL:DD     INDD
     C                   ELSE
     C                   MOVEL     *BLANKS       INDD
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     *BLANKS       INDD
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     *BLANKS       INDD
     C                   ENDIF
     **
      *=============================***
     **
     ** ++++ ** DIARY USER  **
     C                   MOVEA     *ZEROS        FND
     C                   MOVE      *BLANKS       INDU
     C*
     C     'DU_'         SCAN      @@FULL:1      FND                      90
     C     FND(1)        IFGT      0
     C     FND(1)        ADD       3             DU                3 0
     C     ' '           SCAN      @@FULL:DU     DUEND             3 0
     C     DUEND         IFGT      0
     C     DUEND         SUB       DU            XXAAXX            3 0
     C     XXAAXX        ADD       1             DULEN             3 0
     C*
     C     DULEN         IFGT      0
     C     DULEN         SUBST     @@FULL:DU     INDU
     C                   ELSE
     C                   MOVEL     *BLANKS       INDU
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     *BLANKS       INDU
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     *BLANKS       INDU
     C                   ENDIF
     C*
     C     INDU          IFEQ      '@UND    '
     C* IF THE SPECIAL VALUE @UND IS FOUND, GET THE UNDERWRITER FROM THE WMEMP
     **    FILE **
     C     EK            CHAIN     PEM01                              22
     C     *IN22         IFEQ      *OFF
     C                   MOVEL     EMUWTR        STFCD             3
     C     STFCD         CHAIN     PUP01                              23
     C     *IN23         IFEQ      *OFF
     C                   MOVEL     UPUSR         INDU
     C                   ELSE
     C                   MOVEL     *BLANKS       INDU
     C                   ENDIF                                                     *IN23
     C                   ELSE
     C                   MOVEL     *BLANKS       INDU
     C                   ENDIF                                                     *IN22
     C                   ENDIF                                                     @UND
     C*
     C*
     C     INDU          IFNE      *BLANKS
     C     INDU          CHAIN     PUP02                              23
     C     *IN23         IFEQ      *ON
     C                   MOVEL     *BLANKS       INDU                            INVALID USER
     C                   ENDIF                                                     *IN23
     C                   ENDIF                                                    INDU NE BLNK
     **
      *=============================***
     **
     ** ++++ ** DESC. **
     C                   MOVEA     *ZEROS        FND
     C                   MOVE      *BLANKS       INDS
     C*
     C     'DS_'         SCAN      @@FULL:1      FND                      90
     C     FND(1)        IFGT      0
     C     FND(1)        ADD       3             DS                3 0
     C     '.'           SCAN      @@FULL:DS     DSEND             3 0
     C     DSEND         IFGT      1
     C     DSEND         SUB       1             DSEND
     C                   ENDIF
     C     DSEND         IFGT      0
     C     DSEND         SUB       DS            XXAAXX            3 0
     C     XXAAXX        ADD       1             DSLEN             3 0
     C*
     C     DSLEN         IFGT      0
     C     DSLEN         SUBST     @@FULL:DS     INDS
     C                   ELSE
     C                   MOVEL     *BLANKS       INDS
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     *BLANKS       INDS
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     *BLANKS       INDS
     C                   ENDIF
     **
     ** ===================== **
     **
      *
     C                   ENDIF
      *
     C                   ENDSR
      ***
     C**====================================================================**
     *** IMPORT FILE PROCESSING                                           **
     C     TXTIMP        BEGSR
     C**====================================================================**
     C*
      *
      ***        VALIDATE KEY BEFORE PROCESSING
      ***  VALIDATE KEY
     C                   CALL      'ISIV05R'                                    COULD GET FRM FIL
     C                   PARM                    @@KEY            40
     C                   PARM                    @@RTNC            1
      *
      ***        NOT VALID
     C     @@RTNC        IFNE      ' '
     C                   MOVEL     @@RTNC        @@VRCD            2
      *
      *** EXECUTE ERROR SUBROUTINE ***
      *
     C                   EXSR      ERROR1
      *
      * @@ERCD-COPY TO ERROR DIRECTORY, @@DLCD-DELETE FROM ORIGINAL DIRECTRY
      *
      *** FORMAT THE ERROR REPORT DETAIL LINE HERE ***
      *
      *
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEADE
     C                   ENDIF
      *
     C                   EXCEPT    ERR1
      *
     C*** LOOP FOR THE ERRORS
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEADE
     C                   EXCEPT    ERR1
     C                   ENDIF
      *
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     'VR'          $$ERCD            2
     C                   MOVEL     @VR           $$ERDS           80
     C                   EXCEPT    ERR2
      *
     C                   GOTO      ENDTX1
     C                   ENDIF
      *
     C                   MOVEL     ' '           FIRST1            1
     C                   MOVEL     *BLANKS       @@TXCD
     C                   MOVEL     *BLANKS       @@OPCD
     C                   MOVEL     *BLANKS       @@NDCD
     C                   MOVEL     *BLANKS       @@XX              1
     C                   MOVEL     *BLANKS       @@XY              1
     **CALL PGM TO GET FILE DATA INTO A DB FILE
      *
     C                   MOVEL     *BLANKS       @3DIR1
     C                   MOVEL     *BLANKS       @3DIR2
     C     I1IDIR        CAT(P)    '/':0         @3DIR1          132
     C     @3DIR1        CAT(P)    FLOBJN:0      @3DIR2          132
     C*
     C                   CALL      'SCI1DXC'
     C                   PARM      @3DIR2        @3PATH          132
     C                   PARM      *BLANKS       @3RTNC            1
     C*
     C*  IF RTNCD IS NOT *BLANKS, ERROR, PROCESS AS SUCH.
     C**** IF ERROR ON A SINGLE FILE, THEN REPORT ON ERROR REPORT, DON'T FAIL
     C***   ENTIRE .IDX - REPORT SUCCESS ON REPORT AS WELL (INDIV FILES)
     C*********
     C     @3RTNC        IFNE      *BLANKS
     C*  Error on Read of Text File
     C                   MOVEL     'TX'          @@TXCD            2
     C*** This is error on entire idx file must do error routine, and go to
     C***   Next major file.
     C                   EXSR      ERROR1                                        COPY FILES FOR ERRO
     C                   EXSR      ERROR2                                        PRINT ERROR REPORT
     C*
     C                   GOTO      ENDTX1
     C*
     C                   ELSE                                                      @3RTNC
      *
     C* PERFORM OVERRIDE TO MEMBER (Q1JOB)
     C                   Z-ADD     27            I                 5 0
     C                   MOVEA     Q1JOB         OVR(I)
     C                   MOVEA     OVR           CMDSTR          255
     C                   Z-ADD     75            LENGTH           15 5
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    CMDSTR
     C                   PARM                    LENGTH
     C*
     C* OPEN DBF FOR PROCESSING (IF NOT OPEN)
     C                   OPEN      SMIMP                                19
     C*
     C     *IN19         IFEQ      *ON
     C***  Error on File Open
     C*** This is error on entire idx file must do error routine, and go to
     c***   Next major file.
     C                   MOVEL     'OP'          @@OPCD            2
     C                   EXSR      ERROR1                                        COPY FILES FOR ERRO
     C                   EXSR      ERROR2                                        PRNT ERROR RPT
     C                   GOTO      ENDTX1
     C                   ENDIF                                                     *in19 = *ON
     C                   ENDIF                                                     @3RTNC ne
     C*
     C                   READ      SMIMP                                  68
     C     *IN68         DOWEQ     *OFF
      **
     C*** Clear Error fields
     C                   MOVE      *BLANKS       @@IPCD
     C                   MOVE      *BLANKS       @@ADCD
     C*
     C                   MOVEL     IMPATH        @3RMKS            7
     C*
      ****** IF THE IMPATH = '*ADDREMARK" THEN DO ADDITIONAL REMARKS
     C     @3RMKS        IFEQ      '*ADDRMK'
     C**
     C     FIRST1        IFEQ      ' '                                            Can't do addt rmk
     C                   MOVEL     'A2'          @@A2CD            2
     C                   EXSR      ERROR2                                        PRNT ERROR RPT
     C                   GOTO      NXTIM
     C                   ENDIF                                                    end First1
     *** ADDITIONAL REMARKS
     C                   EXSR      ADDRMK
     C*
     C*** check for addrmk error ***
     C     @@ADCD        IFEQ      'AD'
     C                   EXSR      ERROR2
     C                   ENDIF
     C*
     C                   GOTO      NXTIM
     C                   ENDIF
      **
     C*** VALIDATE THE OBJECT EXISTS ***
     C                   CALL      VALIFS
     C                   PARM      IMPATH        @3PTH           256
     C                   PARM      ' '           @3FOU             1
     C*
     C     @3FOU         IFEQ      '0'
     C*** Error Object does not exits
     C** invalid path error
     C                   MOVEL     'IP'          @@IPCD            2
     C* this is an error on one item within the file, don't error whole file
     C                   EXSR      ERROR2
     C                   GOTO      NXTIM
     C                   ENDIF
      **
     C     FIRST1        IFEQ      ' '
     C*** Create Main remark and Copy object
     C*       SET FIRST1 =  '1'
     C                   EXSR      MAINR
     C                   ELSE
     C** Create More Object And Copy Object
     C                   EXSR      MORER
     C                   ENDIF
     ***
     C     NXTIM         TAG
     C                   READ      SMIMP                                  68
     C                   ENDDO
     C     FIRST1        IFEQ      *BLANKS
     C*** Error - No Detail Was Successfully Extracted.
     C                   MOVEL     'ND'          @@NDCD            2
     C                   EXSR      ERROR1                                          CPYF
     C                   EXSR      ERROR2                                          PRTF
     C*
     C                   ELSE
     *** SUCCESSFUL CREATION OF SOMETHING HAS OCCURED AS FIRST1 IS NOT BLANK
     C                   EXSR      SUCS
     C*
     C                   ENDIF
     C*
     C*** CLOSE FILE
     C                   CLOSE     SMIMP                                        19
     *** DLTOVR
     C                   MOVE      *BLANKS       CMDSTR
     C                   MOVEL(P)  DLTOVR        CMDSTR          255
     C                   Z-ADD     75            LENGTH           15 5
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    CMDSTR
     C                   PARM                    LENGTH
     C*
      *
     C     ENDTX1        ENDSR
      ***
     C**====================================================================**
     *** ADDITIONAL REMARKS ROUTINE
     C     ADDRMK        BEGSR
     C**====================================================================**
     C*
     C**** use datastructure to determine how many 75 Byte Rcds to write
     C****
     C                   MOVEL     IMDESC        @3ARMK
     C*
     C     @3AR1         IFNE      *BLANKS
     C                   MOVEL     *BLANKS       @3ARX
     C                   MOVEL     @3AR1         @3ARX            75
     C                   EXSR      WRTAR
     C                   ENDIF
     C*
     C     @3AR2         IFNE      *BLANKS
     C                   MOVEL     *BLANKS       @3ARX
     C                   MOVEL     @3AR2         @3ARX            75
     C                   EXSR      WRTAR
     C                   ENDIF
     C*
     C     @3AR3         IFNE      *BLANKS
     C                   MOVEL     *BLANKS       @3ARX
     C                   MOVEL     @3AR3         @3ARX            75
     C                   EXSR      WRTAR
     C                   ENDIF
     C*
     C     @3AR4         IFNE      *BLANKS
     C                   MOVEL     *BLANKS       @3ARX
     C                   MOVEL     @3AR4         @3ARX            75
     C                   EXSR      WRTAR
     C                   ENDIF
     C***
     C**** IF ERROR ON ADDITIONAL REMARKS
     C**** NO ERRORS   MOVEL'AD'      @@ADCD
     C*
     C                   ENDSR
     C**====================================================================**
      *****************************************************************
      * * *     SUBROUTINE - ERROR2 - ERROR SUBROUTINE 2          * * *
      *****************************************************************
      *
     C     ERROR2        BEGSR
     C*** Error or Success Report here
      *
      ***   ERROR REPORT
      *
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEADE
     C                   ENDIF
      * MAJOR ERROR **************************
     C     @@XX          IFEQ      *BLANKS
     C*
     C                   EXCEPT    ERR1
      *
     C*** LOOP FOR THE ERRORS
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEADE
     C                   EXCEPT    ERR1
     C                   ENDIF
     C                   MOVE      'Y'           @@XX              1
     C*
     C                   ENDIF
     C*
      *
     C     @@TXCD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@TXCD        $$ERCD            2
     C                   MOVEL     @TX           $$ERDS           80
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
      *
     C     @@OPCD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@OPCD        $$ERCD            2
     C                   MOVEL     @OP           $$ERDS           80
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
      *
     C     @@NDCD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@NDCD        $$ERCD            2
     C                   MOVEL     @ND           $$ERDS           80
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
     C     @@ERCD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@ERCD        $$ERCD            2
     C                   MOVEL     @ER           $$ERDS           80
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
     C*
     C     @@DLCD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@DLCD        $$ERCD            2
     C                   MOVEL     @DL           $$ERDS           80
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
     C*
      *
     C     @@IPCD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@IPCD        $$ERCD            2
     C                   MOVEL     @IP           $$ERDS           80
     C                   MOVEL     *BLANKS       XYPTH           132
     C                   MOVEL     IMPATH        XYPTH
     C*
     C     @@XY          IFEQ      *BLANKS
     C                   EXCEPT    ERR2
     C                   MOVE      'Y'           @@XY              1
     C                   ENDIF
     C*
     C                   EXCEPT    ERR2A
     C                   ENDIF
     C*
     C*
      *
     C*
     C     @@ADCD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@ADCD        $$ERCD            2
     C                   MOVEL     @AD           $$ERDS           80
     C                   MOVEL     *BLANKS       XYPTH           132
     C                   MOVEL     IMPATH        XYPTH
     C*
     C     @@XY          IFEQ      *BLANKS
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
     C                   EXCEPT    ERR2A
     C                   ENDIF
     C*
      *
     C*
     C     @@A2CD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@A2CD        $$ERCD            2
     C                   MOVEL     @A2           $$ERDS           80
     C                   MOVEL     *BLANKS       XYPTH           132
     C                   MOVEL     IMPATH        XYPTH
     C*
     C     @@XY          IFEQ      *BLANKS
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
     C                   EXCEPT    ERR2A
     C                   ENDIF
     C*
     C*
     C                   EXCEPT    ERR3
     C*** Begin Add *** 06/25/20 *******************************************
     C                   ExSr      SndEmail
     C*** End   Add *** 06/25/20 *******************************************
      *
     C                   ENDSR
      *
     C**====================================================================**
      *****************************************************************
     C     WRTAR         BEGSR
      *****************************************************************
     C*
     C*          Call API to write a DARTS Additional Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  XXHKEY        Q#KEY            40
     C                   MOVE      Q#SEQ#        Q#SEQ#            5
     C                   MOVEL(P)  @3ARX         Q#CM75           75
     C                   MOVEL(P)  Q#PNAM        Q#PNAM           10
     C                   MOVE      Q#SECL        Q#SECL            3
     C                   MOVEL(P)  Q1USER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  Q#RMCD        Q#RMCD            5
     C                   MOVE      '00000'       Q#ASQ#            5
     C                   MOVEL(P)  Q#CSID        Q#CSID            2
     C*** PUT DATE IN CYMD ORDER ***
     C                   MOVEL     Q#CDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         #SDT              7
     C*
     C                   CALL      'DA701R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#CM75
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#ASQ#
     C                   PARM                    Q#CSID
     C                   PARM                    #SDT
     C**
     C                   ENDSR
     **
     C**====================================================================**
     C     MAINR         BEGSR
     C**====================================================================**
      ***        VALID -
     C****
     ****
      ***             GET NEW NAME FOR THE FILE (QDLS/FOLDER BALANCE)
     C*** EXTENSION FOR THE IMPATH MUST BE FOUND.
     C*
      *
     C                   MOVEL     IMPATH        @3FULL          132
      *
      *
     C     '.'           SCAN      @3FULL:1      XT                3 0    99      FILE EXTN STRT
     C*** GET EXTENTION TO FILE
     C     4             SUBST     @3FULL:XT     @3EXT1            4              FILE EXTNTN
     C                   MOVEL     @3EXT1        WK1               1
     C     WK1           IFNE      '.'
     C                   MOVEL     '.'           @3EXT1
     C                   MOVE      'OBJ'         @3EXT1
     C                   ENDIF
     C*
     C*
      *
     C                   CALL      'SCI107R'
     C                   PARM                    @@WSID
     C                   PARM      '000000'      @@DSEQ            6
     C                   PARM      *BLANKS       @@DSFX            3
     C                   PARM      '0000'        @@BSEQ            4
     C                   PARM      *BLANKS       @@IDOC           12
     C                   PARM      *BLANKS       @@IBCH           12
     C                   PARM      *BLANKS       @@IFLR            8
     C                   PARM      *BLANKS       @@ISFL           12
     C                   PARM                    @3EXT1            4             PASS IN EXTN
     C                   PARM      ' '           @@RTN3            1
      *
      *** IF RTN = 1, DOC INDEX RECORD NO FOUND FOR THIS WSID (OI)
      *
      *
     C     @@RTN3        IFNE      *BLANKS
     C                   MOVEL     @@RTN3        @@DICD            1
     C                   ENDIF
      *
      ***     ONLY PROCESS IF QDLS INFO IS RETRIEVED
     C     @@RTN3        IFEQ      *BLANK
      *
      ***             COPY OBJECT FROM THE IFS FOLDER TO THE NEW QDLS FOLDER
      *
      ***    BUILD TOFLR/OBJ INFO FOR CPYOBJ API CALL
     C                   CALL      'SA030R'
     C                   PARM      *BLANKS       ##SUFX            3
     C                   PARM      *BLANKS       ##PFLR           80
     C*
     C     ##PFLR        IFNE      *BLANKS
      *** Beg chg ***** 11/02/10 ***********************************************
     C*******    '\QDLS\'  CAT  ##PFLR:0  @@WK3  80 P
     C     '\'           CAT(P)    ROOT          RPATH
     C     RPATH         CAT(P)    '\':0         RPATH
     C     RPATH         CAT(P)    ##PFLR:0      @@WK3            80
      *** End chg ***** 11/02/10 ***********************************************
     C                   CAT       '\':0         @@WK3
     C                   CAT       @@IFLR:0      @@WK3
     C                   CAT       '\':0         @@WK3
     C                   CAT       @@ISFL:0      @@WK3
     C                   CAT       '\':0         @@WK3
     C                   CAT       @@IDOC:0      @@WK3
     C                   ELSE
      *** Beg chg ***** 11/02/10 ***********************************************
     C********   '\QDLS\'  CAT  @@IFLR:0  @@WK3  80 P
     C     '\'           CAT(P)    ROOT          RPATH
     C     RPATH         CAT(P)    '\':0         RPATH
     C     RPATH         CAT(P)    @@IFLR:0      @@WK3            80
      *** End chg ***** 11/02/10 ***********************************************
     C                   CAT       '\':0         @@WK3
     C                   CAT       @@ISFL:0      @@WK3
     C                   CAT       '\':0         @@WK3
     C                   CAT       @@IDOC:0      @@WK3
     C                   ENDIF
     C*
     C                   MOVEL     *BLANKS       @3TOBJ
     C                   MOVEL     @@WK3         @3TOBJ
     C                   MOVEL     *BLANKS       @3FOBJ
     C                   MOVEL     IMPATH        @3FOBJ
      *
      *
     C                   CALL      'CPYIFSC'
     C                   PARM                    @3FOBJ
     C                   PARM                    @3TOBJ                         PATH IS READY.. NO D
     C                   PARM      *BLANKS       @3CHR2            2
     C*
     C     '\'           CAT(P)    @@TOBJ:0      WK133           133
     C     @@TDIR        CAT(P)    WK133:0       WK212           212
     C                   MOVEL     WK212         XXTDIR           60
     C                   MOVEL     @3TOBJ        XXTDIR
      *
      *  ** IF RTN CODE  THEN PRINT ON REPORT, NO SUCCESSFUL COPY TO QDLS
      *
     C     @3CHR2        IFNE      *BLANKS
     C                   MOVEL     @3CHR2        @@QDCD            2
     C                   ELSE
     C                   MOVEL     *BLANKS       @@QDCD
     C                   ENDIF
      *
     C     @@QDCD        IFEQ      *BLANKS
      *
      ***             CALL PGM TO UPDATE THE DI AND FB RCDS.. NEXT AVAIL
      *
     C                   CALL      'SCI106R'
     C                   PARM                    @@WSID
     C                   PARM                    @@DSEQ            6
     C                   PARM                    @@BSEQ            4
     C                   PARM                    @@IFLR
     C                   PARM      ' '           @@RTN4            1
      *
     C     @@RTN4        IFNE      *BLANKS
     C                   MOVEL     @@RTN4        @@FBCD            2
     C                   ELSE
     C                   MOVEL     *BLANKS       @@FBCD
     C                   ENDIF
      *
     C     @@FBCD        IFEQ      *BLANKS
      *
      ***             WRITE A DART
     C*
     C                   EXSR      DRTRCD
     C***     IF WE WRITE A DART REMARK, THEN WE SET THIS FLAG FOR LATER PROCESS
     C                   MOVEL     '1'           FIRST1
      *
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C*** Error or Success Report here
      *
     C     @@DICD        IFNE      *BLANK
     C     @@QDCD        ORNE      *BLANK
     C     @@FBCD        ORNE      *BLANK
      ***   ERROR REPORT
      *
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEADE
     C                   ENDIF
      *
     C                   EXCEPT    ERR1
      *
     C*** LOOP FOR THE ERRORS
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEADE
     C                   EXCEPT    ERR1
     C                   ENDIF
      *
     C     @@DICD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@DICD        $$ERCD            2
     C*** BEGIN ADD *** 05/30/08 **************************************
     C     @@DICD        IFEQ      'S'
     C                   MOVEL     @RO           $$ERDS
     C                   ELSE
     C*** END   ADD *** 05/30/08 **************************************
     C                   MOVEL     @DI           $$ERDS           80
     C*** BEGIN ADD *** 05/30/08 **************************************
     C                   ENDIF
     C*** END   ADD *** 05/30/08 **************************************
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
     C*
     C*
     C     @@QDCD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@QDCD        $$ERCD            2
     C                   MOVEL     @QD           $$ERDS           80
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
     C     @@FBCD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@FBCD        $$ERCD            2
     C*** BEGIN ADD *** 05/30/08 **************************************
     C     @@FBCD        IFEQ      'S'
     C                   MOVEL     @RO           $$ERDS
     C                   ELSE
     C*** END   ADD *** 05/30/08 **************************************
     C                   MOVEL     @FB           $$ERDS           80
     C*** BEGIN ADD *** 05/30/08 **************************************
     C                   ENDIF
     C*** END   ADD *** 05/30/08 **************************************
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
      *
     C*
     C                   EXCEPT    ERR3
     C*** Begin Add *** 06/25/20 *******************************************
     C                   ExSr      SndEmail
     C*** End   Add *** 06/25/20 *******************************************
      *
     C                   ELSE
      *
      *
      ***   WRITE SUCCESS REPORT
      *** BEG DEL *** 06/19/06 ****************************
     C*********            EXCPTSUCCS1
      *** END DEL *** 06/19/06 ****************************
      *** BEG ADD *** 06/19/06 ****************************
     C                   MOVE      'S'           QQSTS
     C                   EXSR      WRTLOG
      *** END ADD *** 06/19/06 ****************************
     C                   ENDIF
     C***
     C                   ENDSR
     C**====================================================================**
     C**====================================================================**
     C     MORER         BEGSR
     C**====================================================================**
      ***        VALID -
      ***             GET NEW NAME FOR THE FILE (QDLS/FOLDER BALANCE)
      ********8 GET EXTENSION FROM THE IMPATH
      *** CLEAR WORK FIELDS
      *
     C                   MOVEL     IMPATH        @3FULL          132
      *
      *
     C     '.'           SCAN      @3FULL:1      XT                3 0    99      FILE EXTN STRT
     C*** GET EXTENTION TO FILE
     C     4             SUBST     @3FULL:XT     @3EXT1            4              FILE EXTNTN
     C                   MOVEL     @3EXT1        WK1               1
     C     WK1           IFNE      '.'
     C                   MOVEL     '.'           @3EXT1
     C                   MOVE      'OBJ'         @3EXT1
     C                   ENDIF
     C*
     C*
      *
     C                   CALL      'SCI107R'
     C                   PARM                    @@WSID
     C                   PARM      '000000'      @@DSEQ            6
     C                   PARM      *BLANKS       @@DSFX            3
     C                   PARM      '0000'        @@BSEQ            4
     C                   PARM      *BLANKS       @@IDOC           12
     C                   PARM      *BLANKS       @@IBCH           12
     C                   PARM      *BLANKS       @@IFLR            8
     C                   PARM      *BLANKS       @@ISFL           12
     C                   PARM                    @3EXT1            4
     C                   PARM      ' '           @@RTN3            1
      *
      *** IF RTN = 1, DOC INDEX RECORD NO FOUND FOR THIS WSID (OI)
      *
      *
     C     @@RTN3        IFNE      *BLANKS
     C                   MOVEL     @@RTN3        @@DICD            1
     C                   ENDIF
      *
      ***     ONLY PROCESS IF QDLS INFO IS RETRIEVED
     C     @@RTN3        IFEQ      *BLANK
      *
      ***             COPY OBJECT FROM THE IFS FOLDER TO THE NEW QDLS FOLDER
      *
      ***    BUILD TOFLR/OBJ INFO FOR CPYOBJ API CALL
     C                   CALL      'SA030R'
     C                   PARM      *BLANKS       ##SUFX            3
     C                   PARM      *BLANKS       ##PFLR           80
     C*
     C     ##PFLR        IFNE      *BLANKS
      *** Beg chg ***** 11/02/10 ***********************************************
     C*******    '\QDLS\'  CAT  ##PFLR:0  @@WK3  80 P
     C     '\'           CAT(P)    ROOT          RPATH
     C     RPATH         CAT(P)    '\':0         RPATH
     C     RPATH         CAT(P)    ##PFLR:0      @@WK3            80
      *** End chg ***** 11/02/10 ***********************************************
     C                   CAT       '\':0         @@WK3
     C                   CAT       @@IFLR:0      @@WK3
     C                   CAT       '\':0         @@WK3
     C                   CAT       @@ISFL:0      @@WK3
     C                   CAT       '\':0         @@WK3
     C                   CAT       @@IDOC:0      @@WK3
     C                   ELSE
      *** Beg chg ***** 11/02/10 ***********************************************
     C********   '\QDLS\'  CAT  @@IFLR:0  @@WK3  80 P
     C     '\'           CAT(P)    ROOT          RPATH
     C     RPATH         CAT(P)    '\':0         RPATH
     C     RPATH         CAT(P)    @@IFLR:0      @@WK3            80
      *** End chg ***** 11/02/10 ***********************************************
     C                   CAT       '\':0         @@WK3
     C                   CAT       @@ISFL:0      @@WK3
     C                   CAT       '\':0         @@WK3
     C                   CAT       @@IDOC:0      @@WK3
     C                   ENDIF
     C*
     C                   MOVEL     *BLANKS       @3TOBJ
     C                   MOVEL     @@WK3         @3TOBJ
     C                   MOVEL     *BLANKS       @3FOBJ
     C                   MOVEL     IMPATH        @3FOBJ
      *
      *
     C                   CALL      'CPYIFSC'
     C                   PARM                    @3FOBJ
     C                   PARM                    @3TOBJ                         PATH IS READY.. NO D
     C                   PARM      *BLANKS       @3CHR2            2
     C*
     C     '\'           CAT(P)    @@TOBJ:0      WK133           133
     C     @@TDIR        CAT(P)    WK133:0       WK212           212
     C                   MOVEL     WK212         XXTDIR           60
      *
      *  ** IF RTN CODE  THEN PRINT ON REPORT, NO SUCCESSFUL COPY TO QDLS
      *
     C     @3CHR2        IFNE      *BLANKS
     C                   MOVEL     @3CHR2        @@QDCD            2
     C                   ELSE
     C                   MOVEL     *BLANKS       @@QDCD
     C                   ENDIF
      *
     C     @@QDCD        IFEQ      *BLANKS
      *
      ***             CALL PGM TO UPDATE THE DI AND FB RCDS.. NEXT AVAIL
      *
     C                   CALL      'SCI106R'
     C                   PARM                    @@WSID
     C                   PARM                    @@DSEQ            6
     C                   PARM                    @@BSEQ            4
     C                   PARM                    @@IFLR
     C                   PARM      ' '           @@RTN4            1
      *
     C     @@RTN4        IFNE      *BLANKS
     C                   MOVEL     @@RTN4        @@FBCD            2
     C                   ELSE
     C                   MOVEL     *BLANKS       @@FBCD
     C                   ENDIF
      *
     C     @@FBCD        IFEQ      *BLANKS
      *
      ***             WRITE A DART
     C*
     C                   EXSR      MORCD2
      *
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C*** Error or Success Report here
      *
     C     @@DICD        IFNE      *BLANK
     C     @@QDCD        ORNE      *BLANK
     C     @@FBCD        ORNE      *BLANK
      ***   ERROR REPORT
      *
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEADE
     C                   ENDIF
      *
     C                   EXCEPT    ERR1
      *
     C*** LOOP FOR THE ERRORS
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEADE
     C                   EXCEPT    ERR1
     C                   ENDIF
      *
     C     @@DICD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@DICD        $$ERCD            2
     C*** BEGIN ADD *** 05/30/08 **************************************
     C     @@DICD        IFEQ      'S'
     C                   MOVEL     @RO           $$ERDS
     C                   ELSE
     C*** END   ADD *** 05/30/08 **************************************
     C                   MOVEL     @DI           $$ERDS           80
     C*** BEGIN ADD *** 05/30/08 **************************************
     C                   ENDIF
     C*** END   ADD *** 05/30/08 **************************************
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
     C*
     C*
     C     @@QDCD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@QDCD        $$ERCD            2
     C                   MOVEL     @QD           $$ERDS           80
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
     C     @@FBCD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@FBCD        $$ERCD            2
     C*** BEGIN ADD *** 05/30/08 **************************************
     C     @@FBCD        IFEQ      'S'
     C                   MOVEL     @RO           $$ERDS
     C                   ELSE
     C*** END   ADD *** 05/30/08 **************************************
     C                   MOVEL     @FB           $$ERDS           80
     C*** BEGIN ADD *** 05/30/08 **************************************
     C                   ENDIF
     C*** END   ADD *** 05/30/08 **************************************
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
      *
     C*
     C                   EXCEPT    ERR3
     C*** Begin Add *** 06/25/20 *******************************************
     C                   ExSr      SndEmail
     C*** End   Add *** 06/25/20 *******************************************
      *
     C                   ELSE
      *
      *
      ***   WRITE SUCCESS REPORT
      *** BEG DEL *** 06/19/06 ****************************
     C*********            EXCPTSUCCS1
      *** END DEL *** 06/19/06 ****************************
      *** BEG ADD *** 06/19/06 ****************************
     C                   MOVE      'S'           QQSTS
     C                   EXSR      WRTLOG
      *** END ADD *** 06/19/06 ****************************
     C                   ENDIF
     C***
     C                   ENDSR
      *****************************************************************
      * * *     SUBROUTINE - MORCD2 - CREATE MORE OBJ RECORD       * * *
      *****************************************************************
      *
     C     MORCD2        BEGSR
      *
      *
     C                   MOVEL     'O'           $$OTYP
     C*
     C                   CALL      'ISEWMD'
     C                   PARM      @@KEY         $$KEY            40                 ????
     C                   PARM      Q#SEQ#        $$SEQ             5
     C*
     C                   PARM      @@IDOC        $$OID            40
     C                   PARM      Q#FOLD        $$OFLR           60
     C                   PARM                    $$OTYP            1
     C                   PARM      IMDESC        $$ODSC           40
     C                   PARM      Q#RMCD        $$RMCD            5
     C                   PARM      'AS400   '    $$SYSN           20
     C                   PARM      ##PNAM        $$PNAM           10
     C                   PARM      Q1USER        $$USR            10
     C                   PARM      ' '           $$RTN             1
     C                   PARM      *BLANKS       $$ERTN           80
     C*
      *
     C                   ENDSR
     ***
     C     SUCS          BEGSR
     C*
     C     I1ADIR        IFNE      *BLANKS
      *
     C                   TIME                    DSDTTM
     C                   MOVE      DSDATE        $$DAT             6
     C                   MOVE      DSTIME        $$TIM             6
     C                   MOVEL     $$DAT         $$DTTM           12
     C                   MOVE      $$TIM         $$DTTM
     C*
     C     $$DTTM        CAT(P)    FLOBJN:0      $$NOBJ          132
     C*
      *
     C***  END ADD ***8 02/27/03 ********8
      *
     C                   CALL      'CPYOBJC'
     C                   PARM      I1IDIR        @@FDIR           80
     C                   PARM      FLOBJN        @@FOBJ          132
     C                   PARM      I1ADIR        @@TDIR           80
     C**** CHG 02/27/03 ** PARM FLOBJN    @@TOBJ132
     C                   PARM      $$NOBJ        @@TOBJ          132
     C                   PARM      '  '          @@RTN2            2
      *
      *  ** IF RTN CODE  THEN PRINT ON REPORT, NO ARCHIVE!!!
      *
     C     @@RTN2        IFNE      *BLANKS
     C                   MOVEL     @@RTN2        @@ARCD            2
     C                   ELSE
     C                   MOVEL     *BLANKS       @@ARCD
     C                   ENDIF
      *
     C                   ENDIF                                                   IF I1ADIR
      *
     C     @@ARCD        IFEQ      *BLANKS
      *
      *** ONLY DELETE IF THE COPY ABOVE WAS SUCCESSFUL (TO ERROR DIRECTORY)
      *
     C                   CALL      'DELOBJC'
     C                   PARM      I1IDIR        @@FDIR           80
     C                   PARM      FLOBJN        @@FOBJ          132
     C                   PARM      '  '          @@RTN2            2
      *
     C     @@RTN2        IFNE      *BLANKS
     C                   MOVEL     @@RTN2        @@DLCD            2
     C                   ELSE
     C                   MOVEL     *BLANKS       @@DLCD
     C                   ENDIF
      *
     C                   ENDIF                                                  @@ARCD
      *
      *
     C*** Error or Success Report here
      *
     C     @@ARCD        IFNE      *BLANK
     C     @@DLCD        ORNE      *BLANK
      ***   ERROR REPORT
      *
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEADE
     C                   ENDIF
      *
     C                   EXCEPT    ERR1
      *
     C*** LOOP FOR THE ERRORS
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEADE
     C                   EXCEPT    ERR1
     C                   ENDIF
      *
      *
     C*
     C     @@ARCD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@ARCD        $$ERCD            2
     C                   MOVEL     @AR           $$ERDS           80
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
     C*
     C     @@DLCD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@DLCD        $$ERCD            2
     C                   MOVEL     @DL           $$ERDS           80
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
     C*
     C                   EXCEPT    ERR3
     C*** Begin Add *** 06/25/20 *******************************************
     C                   ExSr      SndEmail
     C*** End   Add *** 06/25/20 *******************************************
      *
     C                   ELSE
      *
      *
      ***   WRITE SUCCESS REPORT
      *** BEG DEL *** 06/19/06 ****************************
     C*********            EXCPTSUCCS1
      *** END DEL *** 06/19/06 ****************************
      *** BEG ADD *** 06/19/06 ****************************
     C                   MOVE      'S'           QQSTS
     C                   EXSR      WRTLOG
      *** END ADD *** 06/19/06 ****************************
     C                   ENDIF
     C*
     C                   ENDSR
     C*** ADD 6/14/06 ***********************************
     C***
     C**====================================================================**
     C*** END ADD 6/14/06 ***********************************
      *** BEG ADD *** 06/19/06 ****************************
      *** Begin Add *** 06/25/20 *******************************************
      *****************************************************************
      * SNDEMAIL - Send email if error occurred
      *****************************************************************
      /free
       BegSr SndEmail;

          // Get the email sender
          Chain 'S' per00;
          If %Found(smerl);
             inFromEmail = ERemail;
          EndIf;

          // Get the email recipients
          Clear emailRecDs;
          Read per00;
          DoW Not %Eof(smerl);
             count += 1;
             emailRec(count) = ERemail;
             Read per00;
          EndDo;
          inToEmail    = emailRecDs;

          // Build the subject line
          inSubject    = 'Object "' + %Trim(flObjN) + '" failed to import.';

          // Build the message
          If $$nObj <> ' ';
             inBody       = %Trim(I1iDir) + '/"' + %Trim(flObjN) +
                          '" failed to import.  The object was copied to ' +
                          %Trim(I1eDir) + '/"' + %Trim($$nObj) + '".  ' +
                          'Error: ' + %Trim($$erCd) + '-' + %Trim($$erDs);
          Else;
             inBody       = %Trim(I1iDir) + '/"' + %Trim(flObjN) +
                          '" failed to import. ' +
                          'Error: ' + %Trim($$erCd) + '-' + %Trim($$erDs);
          EndIf;

          // Get the client suffix
          GetSuffix(outSuffix:outFolderPrfx);
          inSuffix     = outsuffix;

          // Send the email message if there are email recipients
          If inToEmail <> ' ';
             SendEmail(inFromEmail:inToEmail:inSubject:inBody:inAttach:
                    dsDarts:dsDataQ:inAuto:inProgram:inIncJobInfo:inSuffix);
          EndIf;

       EndSr;
      /end-free
      *** End   Add *** 06/25/20 *******************************************
      *
      *****************************************************************
      * * *     SUBROUTINE - MORCD - CREATE MORE OBJ RECORD       * * *
      *****************************************************************
      *
     C     WRTLOG        BEGSR
      *
     C                   MOVE      *BLANKS       QQADIR
     C                   MOVE      *BLANKS       QQEDIR
     C                   MOVE      *BLANKS       QQERCD
     C                   MOVE      *BLANKS       QQERDE
      *
     C                   MOVE      $$DAT         QQDATE
     C                   MOVE      $$TIM         QQTIME
      *
     C     QQSTS         IFEQ      'S'
     C                   MOVE      I1ADIR        QQADIR
     C                   MOVEL(P)  I1ADIR        QQTDIR
     C                   ENDIF
      *
     C     QQSTS         IFEQ      'U'
     C                   MOVE      I1EDIR        QQEDIR
     C                   MOVE(P)   I1EDIR        QQTDIR
     C                   MOVE      $$ERCD        QQERCD
     C                   MOVE      $$ERDS        QQERDE
     C                   ENDIF
      *
     C                   CALL      'DA002R'
     C                   PARM                    QQDATE            6 0
     C                   PARM                    QQTIME            6 0
     C                   PARM      I1IDIR        QQFDIR           80
     C                   PARM      FLOBJN        QQFFIL          132
     C                   PARM                    QQEDIR           80
     C                   PARM                    QQADIR           80
     C                   PARM                    QQTDIR           80
     C                   PARM      $$NOBJ        QQTFIL          132
     C                   PARM                    QQERCD            2
     C                   PARM                    QQERDE           80
     C                   PARM                    QQSTS             1
     C                   PARM      Q1PGM         QQPGM            10
     C                   PARM                    QQINLR            1
      *
     C                   ENDSR
      *
      *** END ADD *** 06/19/06 ****************************
      **** ADD 2/12/07 ********************************************
      *===================================================================
     C     VALKY1        BEGSR
      *===================================================================
      *
      ***        VALIDATE KEY IN SAME SUBROUTINE
      ***  VALIDATE KEY
     C                   CALL      'ISIV05R'                                    COULD GET FRM FIL
     C                   PARM                    @@KEY            40
     C                   PARM      *BLANK        @@RTNC            1
     **** CHG ADDED *BLANK TO THE PARM VALUE ABOVE 2/12/07 ****************
      *
      ***        NOT VALID
     C     @@RTNC        IFNE      ' '
     C                   MOVEL     @@RTNC        @@VRCD            2
      *
      *** EXECUTE ERROR SUBROUTINE ***
      *
     C                   EXSR      ERROR1
      *
      * @@ERCD-COPY TO ERROR DIRECTORY, @@DLCD-DELETE FROM ORIGINAL DIRECTRY
      *
      *** FORMAT THE ERROR REPORT DETAIL LINE HERE ***
      ***   ERROR REPORT
      *
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEADE
     C                   ENDIF
      *
     C                   EXCEPT    ERR1
      *
     C*** LOOP FOR THE ERRORS
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEADE
     C                   EXCEPT    ERR1
     C                   ENDIF
     C*
     C*
     C     @@VRCD        IFNE      *BLANK
     C                   MOVEL     *BLANKS       $$ERCD
     C                   MOVEL     *BLANKS       $$ERDS
     C                   MOVEL     @@VRCD        $$ERCD            2
     C                   MOVEL     @VR           $$ERDS           80
      *** BEG ADD *** 06/19/06 ****************************
     C                   MOVE      'U'           QQSTS
     C                   EXSR      WRTLOG
      *** END ADD *** 06/19/06 ****************************
     C                   EXCEPT    ERR2
     C                   ENDIF
     C*
     C                   EXCEPT    ERR3
     C*** Begin Add *** 06/25/20 *******************************************
     C                   ExSr      SndEmail
     C*** End   Add *** 06/25/20 *******************************************
      *
      **** MUST EXIT PROCESSING IF ERROR OCCURED.
      ***
     C                   ENDIF
     C                   ENDSR
      **** ADD 2/12/07 ********************************************
     O*
     OQSYSPRT1  E            HEADE          1 06
     O                                          111 'OBJECT IMPORT ERROR LIST'
     O                       DSDATE        Y     12
     O                       DSTIME             196 '  :  :  '
     O          E            HEADE          3
     O                                           14 'FROM DIR:'
     O                       I1IDIR              95
     O*
     O                                          110 'ERROR DIR:'
     O                       I1EDIR             191
      *
     O          E            HEADE          1
     O                                           10 'FILE NAME'
     O                                          144 'TO FILE'
     O          E            ERR1           1
     O                       FLOBJN             133
     O                       XXTDIR             198
     O          E            ERR2           1
     O                                            8 '     ***'
     O                       $$ERCD              11
     O                                           12 '-'
     O                       $$ERDS              92
     C*** Str ADD 6/14/06 ***********************************
     O          E            ERR2A          1
     O                                           13 '     ***'
     O                                           37 ' Invalid IDX Value     '
     O                                           38 '-'
     O                       XYPTH              178
     C*** End ADD 6/14/06 ***********************************
     O          E            ERR3           1
      *** BEG DEL *** 06/19/06 ****************************
     O*QSYSPRT2E  106 ********* HEADS
     O*******
     O*******                           111 'OBJECT IMPORT PROCESSED '
     O*******                  DSDATEY   12
     O*******                  DSTIME   196 '  :  :  '
     O******* E  3             HEADS
     O*******                            14 'FROM DIR:'
     O*******                  I1IDIR    95
     O*******
     O*******                           112 'ARCHIVE DIR:'
     O*******                  I1ADIR   193
     O*******
     O******* E  1             HEADS
     O*******                            10 'FILE NAME'
     O*******                           144 'TO FILE'
     O******* E  1             SUCCS1
     O*******                  FLOBJN   133
     O*******                  XXTDIR   198
      *** END DEL *** 06/19/06 ****************************
**  (OVR) QCMDEXC COMMAND            ADD - 6/14/06
OVRDBF  FILE(SMIMP)   MBR(          )
