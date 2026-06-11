     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H BNDDIR('WTBNDDIR')
     H*****************************************************************
     H/TITLE W0170R - PROCESS CANCELLATION CONFIRMATIONS
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  CREATED: 09/21/98  PROGRAMMER: C:87  PROJECT: WC762          *
     H*****************************************************************
     H* REVISIONS:                                                    *
      *                                                               *
      * 10/28/20  C:079 Project: WC2671                               *
      *                 Add Audit Request Fields                      *
      *                                                               *
      * 02/06/18  C:33  Support: SY371                                *
      *                 Don't prompt for output control when cancel   *
      *                  is pressed                                   *
      *                                                               *
      * 10/06/16  C:65  PROJECT: SY371                                *
      *                 Implement Scriptura output control window     *
      *                 when process is flagged to generate Scriptura *
      *                 output.  The settings are stored/passed via   *
      *                 a local data area which is picked up by       *
      *                 API's and the printing program.               *
     H*                                                               *
     H* 01/23/14  C:191  Project: SY336 Priss# 15
     H*                  SSN text box not showing up.
     H*                  Changes made to handle the processing.
     H*                                                               *
     H* 10/01/10  C:33   Project: SY190 Priss# 76 (93WIF)             *
     H*                  SSN display control for CLT093.              *
     H*                                                               *
     H* 05/03/10  C:220  PROJECT: WC1881                              *
     H*                  Add Ctrl flag to Final Audit Request Form    *
     H*                                                               *
     H* 07/28/09  C:189  PROJECT: 93WIF                               *
     H*                  CHANGE SSN DISPLAY ON AUDIT REQUEST          *
     H*                                                               *
     H*  12/18/08  C:33  Project: 102WEP   WA102/66472                *
     H*                  Remove change from 09/25/08.                 *
     H*                                                               *
     H*  09/25/08  C:33  Seq#: WA102/65344                            *
     H*                  Use different parm list to call W0170C.      *
     H*                                                               *
     H*  10/03/07  C:193 PROJECT: 71WIQ                               *
     H*                  INCL ECM PREPRTED FORM/BASE AUDIT FORM       *
     H*                                                               *
     H* 11/10/04 C:09  SUPPORT WA033 44548                            *
     H*                Modify MODE parameter on call to W0170C to     *
     H*                ensure job is reflected to run on demand       *
     H*                and route output to user parm output queue     *
     H*                                                               *
     H* 12/11/02 C:61  SUPPORT WA081 35144                            *
     H*                Get most recent ACTIVE CX reason code record.  *
     H*                                                               *
     H* 06/28/02 C:61  SUPPORT WA099 33211                            *
     H*                Do not display "Postion to" employer division  *
     H*                field.                                         *
     H*                                                               *
     H* 05/09/01  C:136  PROJECT: WC762                               *
     H*                  CHANGED OPT 9 FROM 'CONFIRM' TO 'CONFIRM/    *
     H*                  UNCONFIRM'                                   *
     H*                                                               *
     H* 03/05/01  C:35   PROJECT: SUPPORT WA114 #26561                *
     H*                  REMOVE KEYSET CODE FROM PGMSTR SO THAT       *
     H*                  CANCELLATIONS EFFECTIVE PRIOR TO CURRENT     *
     H*                  DATE WILL SHOW UPON ENTRY OF SCREEN.         *
     H*                                                               *
     H* 11/01/00  C:35   PROJECT: WC577                               *
     H*                  ADD OPTION FOR A/R INQUIRY.                  *
     H*                                                               *
     H*  10/21/99  C:87  PROJECT: WC577                               *
     H*                  Use WTCRP instead of WTABL to get info for   *
     H*                  the cancellation reason.                     *
     H*                                                               *
     H* 03/23/99  C:35   PROJECT: SUPPORT                             *
     H*                  DISPLAY WINDOW FOR AUDIT REQUEST AND         *
     H*                  CONFIRMATION NOTICE IF EITHER FORM IS        *
     H*                  FLAGGED TO BE GENERATED.                     *
     H*                                                               *
     H*****************************************************************
     H*
     FW0170F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWDCXP     IF   E             DISK
     F                                     RENAME(PCX01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWDCXL6    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCX01:DBRCDF)
     F*
     F*  Cancellation confirmation control file
     FWMCFL     IF   E           K DISK
     F*
     F*  Employer master file
     FWMEML     IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F*
     F*  Employer detail file
     FWDELL     IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F*
     FWMEXL     IF   E           K DISK
     F*
     F*  Table file
     F*
     FWTCRL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     F*
     FSTCTL1    IF   E           K DISK
     d/copy sPrLetter
     D/copy sPrXML
     D isUseScript     S              1n   inz
     d processType     c                   const('Cancellations')
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1
     d
     D SVLDA           DS          1024    DTAARA(*LDA)
     D xxDart                         1    overlay(svlda:251)
     d xxRMCD                         5    overlay(svlda:252)
     d xxCopy                         1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d xxBody                       350    overlay(svlda:673)
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(16) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(16) ALT(OF)
     D*
     D*
     D*
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Cancellation Confirm-
     D                                     ations')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W0170R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D CHGNUM          C                   CONST('SA020R')
     D INFOMX          C                   CONST(2)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(11)
     D DELETD          C                   CONST('*Deleted')
     D*
     D*  Data structures for view
     D*
     D                 DS
     D  XXVIEW                 1     22
     D  XXCXDT                 1     10
     D  XXEMP#                14     22
     D*  Data structures for info changes
     D                 DS
     D  XXINF1                 1     45
     D  X1CFDT                 6     15
     D  X1ENAM                19     45
     D                 DS
     D  XXINF2                 1     45
     D  X2CDAT                 6     15
     D  X2RSN                 19     20
     D  X2RSNM                22     45

     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C                   PARM                    ##OUTQ           10
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*
     C*  Issue confirmations parm list
     C     ISCOPL        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM      SCADTK        @@ADTK            1
     C                   PARM      SCAURQ        @@AURQ            5
     C                   PARM      SCPCHL        @@PCHL            1
     C                   PARM                    @@QUE            10
     C                   PARM                    @@MODE            1
     C                   PARM      'B'           @@AFTY            1
     C                   PARM      '00000000'    @@DDUE            8
     C                   PARM      SCASSF        QQASSF            1
     C                   PARM      SCSRFL        @@SRFL            1
     C*** Beg Add *** 10/28/20 ****************************************
     C                   PARM      SCg301E       @@G301E           1
     C                   PARM      SCg311E       @@G311E           1
     C                   PARM      SCgAsRd       @@GasRd           1
     C*** End Add *** 10/28/20 ****************************************
     C*
     C     ISCOP2        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM      SCADTK        @@ADTK            1
     C                   PARM      SCAURQ        @@AURQ            5
     C                   PARM      SCPCHL        @@PCHL            1
     C                   PARM                    @@QUE            10
     C                   PARM                    @@MODE            1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKCXDT
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C     CXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*  Cancellation confirmation key list
     C     CFKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*  Employer master key list
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C*  Employer detail key list
     C     ELKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
     C*
     C*
     C     CRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKST
     C                   KFLD                    KKKEY
     C*                    KFLD           KKEFDT
     C     CRKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKST
     C                   KFLD                    KKKEY
     C*
     C*  Rate Set Code file
     C     RSKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKRTCD
     C*
     C*  Admin/DARTS control file
     C     EXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CXCO#         KKCO#
     C     *LIKE         DEFINE    CXFND         KKFND
     C     *LIKE         DEFINE    CXCXDT        KKCXDT
     C     *LIKE         DEFINE    CXEMP#        KKEMP#
     C     *LIKE         DEFINE    CXDIV         KKDIV
     C     *LIKE         DEFINE    CXFYR         KKFYR
     C     *LIKE         DEFINE    CRKEY         KKKEY
     C     *LIKE         DEFINE    CREFDT        KKEFDT
     C     *LIKE         DEFINE    RSRTCD        KKRTCD
     C     *LIKE         DEFINE    CRST          KKST
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for the F11=Issue confirmations screen
     C*================================================================
     C     ERRCK3        BEGSR
     C*
     C     *IN98         IFEQ      *ON
     C*
     C*     Check flag is 'Y' or 'N'
     C     SCADTK        IFNE      'Y'
     C     SCADTK        ANDNE     'N'
     C                   SETON                                        71  99
     C                   ENDIF
     C*
     C*     Check DARTS remark code is valid
     C  N99SCAURQ        IFNE      *BLANKS
     C     SCAURQ        CHAIN     PCT01                              72
     C     *IN72         IFEQ      *ON
     C                   SETON                                        99
     C                   ENDIF
     C                   ENDIF                                                  SCAURQ=*BLANKS
     C*
     C                   ENDIF                                                  *IN98=*ON
     C*
     C     *IN97         IFEQ      *ON
     C*
     C*     Check flag is 'Y' or 'N'
     C  N99SCPCHL        IFNE      'Y'
     C     SCPCHL        ANDNE     'N'
     C                   SETON                                        73  99
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN97=*ON
     C*
     C*
     C                   SETOFF                                       8081
     C     USESS#        IFEQ      *ON
     C  N99SCASSF        IFNE      ' '
     C     SCASSF        IFNE      'Y'
     C     SCASSF        ANDNE     'N'
     C     SCASSF        ANDNE     '4'
     C                   SETON                                        81  99
     C                   ENDIF
     C                   ELSE
     C                   SETON                                        80  99
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C  N99SCSRFL        IFNE      ' '
     C     SCSRFL        ANDNE     'Y'
     C                   SETON                                        63  99
     C                   ENDIF
     C*
     C*** END ADD *** 05/03/10 ***************************************
     C                   ENDSR                                                  ERRCK3
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C                   Z-ADD     ##CO#         SCCO#
     C                   Z-ADD     ##FND         SCFND
     C                   MOVEL     ##FNAM        SCFNNM
     C*
     C                   Z-ADD     ##CO#         KKCO#
     C                   Z-ADD     ##FND         KKFND
     C*
     C     CFKY          CHAIN     PCF01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     CFCFSD        KKCXDT
     C                   Z-ADD     CFCFSD        SSCXDT            8 0          subset field
     C                   ELSE
     C                   Z-ADD     0             KKCXDT
     C                   Z-ADD     0             SSCXDT
     C                   ENDIF
     C*
     C                   MOVE      *LOVAL        KKEMP#
     C                   MOVE      *LOVAL        KKDIV
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   SELECT
     C*
     C     *IN21         WHENEQ    *ON                                          VIEW#1
     C                   MOVE      SCPST1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKCXDT
     C*
     C                   MOVE      SCPS1A        KKEMP#
     C                   MOVE      SCPS1B        KKDIV
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVE      SCCXDT        KKCXDT
     C                   MOVE      SCEMP#        KKEMP#
     C                   MOVE      SCDIV         KKDIV
     C*
     C                   ENDSR                                                  KEYSFL
     C*================================================================
     C* KGDO - Process F7=Issue confirmations
     C*================================================================
     C     KGDO          BEGSR
     C*
     C*  Set indicators on if Generate Adt Rqst flag (CFAURQ) = 'Y' and if
     C*  Generate Confirmation Notice flag (CFNOTC) = 'Y'
     C     CFKY          CHAIN     PCF01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     CFAURQ        IFEQ      'Y'
     C                   MOVE      *ON           *IN98
     C                   ELSE
     C                   MOVE      *OFF          *IN98
     C                   ENDIF
     C*
     C     CFNOTC        IFEQ      'Y'
     C                   MOVE      *ON           *IN97
     C                   ELSE
     C                   MOVE      *OFF          *IN97
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN60
     C*
     C*  Default screen 4 values
     C*
     C                   MOVE      'Y'           SCADTK
     C                   MOVE      'Y'           SCPCHL
     C*
     C                   MOVE      *BLANKS       SCAURQ
     C     EXKY          CHAIN     PEX01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EXAURQ        SCAURQ
     C                   MOVEL     EXASSF        SCASSF
     C                   ENDIF
     C*
     C* Set display attributes for screen
      /Free
       visOn93 = *off;
       disSSN = *off;
      /End-free
     C                   SELECT
     C     CLTID         WHENEQ    '093'
     C                   MOVE      X'20'         AT1A                           Normal
      /Free
       visOn93 = *on;
       disSSN = *off;
      /End-free
     C                   MOVE      *ON           USESS#            1
     C                   OTHER
     C                   CLEAR                   SCASSF
     C                   MOVE      X'A7'         AT1A                           Non-Dsp Prot
      /Free
       visOn93 = *off;
       disSSN = *on;
      /End-free
     C                   MOVE      *OFF          USESS#            1
     C                   ENDSL
     C*
     C     *IN98         IFEQ      *ON
     C     *IN97         OREQ      *ON
     C*
     C*  Output screen
     C     *IN99         DOUNE     '1'
     C*
     C                   EXFMT     SCR04
     C*
     C                   MOVE      *OFF          *IN99
     C*
     C     *INKL         IFEQ      *OFF
     C                   EXSR      ERRCK3
     C                   ENDIF
     C*
     C                   ENDDO                                                  *IN99 NE '1'
     C*
     C                   ENDIF                                                  *IN98
      /free

       If isUseScript and not *inkl;

          xxProcType = processType;
          xxMass = 'Y';

          In *Lock *dtaara;

          Clear svLDA;

          Chain(n) cfky pcf01;

          Select;
          When %found and CFDART = 'Y' and CFCFDR <> ' ';
             xxDart = 'Y';
             xxRMCD = CFCFDR;
          Other;
             xxDart = 'N';
             xxRMCD = ' ';
          Endsl;

          Out *dtaara;

          xxSubmit = OutputWindow(xxProcType:xxMass);

          If xxSubmit = *off;
             *inkl = *on;
          Endif;

       Endif;

      /end-free
     C*
     C*  Call W0170C and pass parms then display screen that print has
     C*  been submitted to batch
     C     *INKL         IFEQ      *OFF
     C*
     C                   MOVE      CXCO#         @@CO#
     C                   MOVEL     CXFND         @@FND
     C                   MOVEL     ##OUTQ        @@QUE
     C                   MOVE      'D'           @@MODE
     C*
     C                   SELECT
     C                   OTHER
     C                   CALL      'W0170C'      ISCOPL
     C                   ENDSL
     C*
     C                   CALL      'WA007R'
     C*
     C                   ENDIF                                                  *INKL=*OFF
     C*
     C                   ENDSR                                                  KGDO
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     CXCXDT        KKCXDT
     C                   Z-ADD     CXEMP#        KKEMP#
     C                   Z-ADD     CXDIV         KKDIV
     C*
     C*  Get employer master record
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     EMNAME        X1ENAM
     C                   ENDIF
     C*
     C*  Get cancellation reason
     C                   MOVE      CXRSN         X2RSN
     C                   MOVEL     CXRSN         KKKEY
     C*
     C*
     C                   MOVE      *BLANKS       KKST
     C                   Z-ADD     CXFYR         KKFYR
     C*
     C*    Get rate set code
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ELRTCD        KKRTCD
     C*    Get jurisdiction state
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RSJRST        KKST
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   Z-ADD     CXCDAT        KKEFDT
      *
     C     CRKY          SETGT     PCR01
     C     CRKY1         READPE    PCR01                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     CRSTAT        ANDEQ     'I'
     C     CRKY1         READPE    PCR01                                  60
     C                   ENDDO
      *
     C*
     C   60              MOVE      *BLANKS       X2RSNM
     C   60              MOVE      *BLANKS       X2RSN
     C  N60              MOVEL     CRDESC        X2RSNM
     C*
     C*  Format date fields
     C                   MOVEL     'DATE'        EDTMSK
     C*
     C*               Cancel date
     C                   Z-ADD     CXCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXCXDT
     C*
     C*               Cancellation confirmed date
     C                   Z-ADD     CXCFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         X1CFDT
     C*
     C*               Cancellation issued date
     C                   Z-ADD     CXCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         X2CDAT
     C*
     C*  Edit employer and division numbers
     C                   MOVE      *BLANKS       EDTMSK
     C*
     C                   MOVEL(P)  EMEMP#        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXEMP#
     C*
     C                   MOVEL(P)  EMDIV         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C*
     C*  Move record values into hidden subfile screen fields
     C                   Z-ADD     CXCXDT        SCCXDT
     C                   Z-ADD     CXEMP#        SCEMP#
     C                   Z-ADD     CXDIV         SCDIV
     C                   Z-ADD     CXFYR         SCFYR
     C*
     C*  Load SCDATA according to INFO#
     C                   MOVEL(P)  XXVIEW        SCDATA
     C*
     C                   SELECT
     C*
     C     INFO#         WHENEQ    1
     C                   MOVE      XXINF1        SCDATA
     C*
     C     INFO#         WHENEQ    2
     C                   MOVE      XXINF2        SCDATA
     C*
     C                   ENDSL
     C*
     C*
     C                   Z-ADD     ##CO#         KKCO#
     C                   Z-ADD     ##FND         KKFND
     C*
     C     CFKY          CHAIN     PCF01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CFSRFL        SCSRFL
       //*** Beg Add *** 10/28/20 *************************************
       scg301E = CFg301E;
       scg311E = CFg311E;
       scGaSrD = CFgAsRd;
       //*** End Add *** 10/28/20 *************************************
     C                   ELSE
     C                   MOVEL     *BLANK        SCSRFL
     C                   ENDIF
     C*
     C*
     C                   ENDSR                                                  MVDBSC
     C*================================================================
     C* OPT9 - Subroutine to process option 9
     C*================================================================
     C     OPT9          BEGSR
     C*
     C                   MOVEL     'S'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL
     C*
     C                   ENDSR                                                  OPT9
     C*================================================================
     C* OPT60 - Subroutine to process option 60
     C*================================================================
     C     OPT60         BEGSR
     C*
     C                   MOVEL     '1'           ##CALL
     C*
     C                   Z-ADD     SCEMP#        KKEMP#
     C                   Z-ADD     SCDIV         KKDIV
     C                   CLEAR                   DBRRN2
     C     EMKY          CHAIN     PEM01                              60
     C*
     C                   CALL      'W0612R5'                            99
     C                   PARM                    ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      DBRRN2        ##RRN             9 0
     C*
     C                   ENDSR                                                  OPT60
     C*================================================================
     C* OPT62 - Subroutine to process option 62
     C*================================================================
     C     OPT62         BEGSR
     C*
     C                   MOVEL     '1'           ##CALL
     C*
     C                   Z-ADD     SCEMP#        KKEMP#
     C                   Z-ADD     SCDIV         KKDIV
     C                   Z-ADD     SCFYR         KKFYR
     C     EMKY          CHAIN     PEM01                              60
     C     ELKY          CHAIN     PEL01                              60
     C*
     C                   CALL      'W0168R'
     C                   PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM                    ##CALL            1
     C                   PARM      DBRRN2        ##RRN             9 0          WMEMP
     C                   PARM      DBRRN3        ##RRN2            9 0          WDELP
     C*
     C                   ENDSR                                                  OPT62
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKG         CASEQ     *ON           KGDO                           <F11>
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '9'           OPT9
     C     SCSEL         CASEQ     '60'          OPT60
     C     SCSEL         CASEQ     '62'          OPT62
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      /free

       isUseScript = UseScriptura(processType);

      /end-free
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
     C   21CXKY          READE     DBRCDF                                 95
     C*
     C                   ENDSR                                                  READ
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*
     C   21CXKY          READPE    DBRCDF                                 95
     C*
     C                   ENDSR                                                  READP
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*
     C   21KEY1          SETGT     DBRCDF                             96
     C*
     C                   ENDSR                                                  SETGT
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*
     C   21KEY1          SETLL     DBRCDF                             96
     C*
     C                   ENDSR                                                  SETLL
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C                   Z-ADD     CXEMP#        KKEMP#
     C                   Z-ADD     CXDIV         KKDIV                          Include only
     C     EMKY          CHAIN     PEM01                              60        state-report
     C     EMSTRP        IFNE      'Y'                                          ing div's in
     C                   MOVE      *OFF          MEETSS                         subset.
     C                   ENDIF
     C                   SELECT
     C*
     C     SSCXDT        WHENNE    *ZEROS
     C     CXCXDT        ANDLT     SSCXDT
     C                   MOVE      *OFF          MEETSS
     C*
     C*   Do not display if reinstated
     C     CXREIN        WHENEQ    'Y'
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDSL
     C*
     C                   ENDSR
** Column headings
Opt  CX Eff Dt   Employer#     Confirm Dt   Employer DBA
Opt  CX Eff Dt   Employer#    CX Issue Dt   Cancellation Reason
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                YYY
1099=Confirm/Unconfirm      YNN
16060=A/R Inquiry           YYY
16262=CX History            YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
207F7=Issue confirmations   YNN
209F9=Change info           YYY
212F12=Cancel               NNY
213F13=Chg user parms       YYN
217F17=Subset               NNN
3
