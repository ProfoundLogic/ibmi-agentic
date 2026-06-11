     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE SC001R - STANDALONE Add-a-Remark Screen                 **
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This allow a user to create an "ad hoc" darts remark.   **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**            - Any Interactive application                    **
     H**   CALLS TO:                                                 **
     H**     SADDDOC                                                 **
     H**     SDSPFLR                                                 **
     H**     SCR41R                                                  **
     H**     SC530R                                                  **
     H**     SC580R                                                  **
     H**     SCEXITC                                                 **
     H**     SRKCVT                                                  **
     H**     SCR14R                                                  **
     H**     SRDCK                                                   **
     H**     CTDEPG                                                  **
     H**     SVALDOC                                                 **
     H**     SVALFLR                                                 **
     H**   PROGRAMMER: C:41                                          **
     H**   CREATED:   12/04/95      PROJECT: DR016                   **
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  04/17/15 C:062 Support: WC025 86868 (31712)
     H*                 Don't populate the name into the darts key
     H*
     H*  12/19/12 C:282 Project: SY266
     H*                 Added the functionality of AV_KEYS.
     H*
     H*  05/31/11 C:190 Project: 25WPW
     H*              2. Default DEPT & Type Code if remark code = 'CLWCD'
     H*              3. Default Diary type = 'Y' if remark code = 'CLWCD'
     H*              3. Default Diary user = ' ' if remark code = 'CLWCD'
     H*
     H*  10/17/11 C:277 Project: SY195
     H*                 QDLS migration to IFS
     H*                 Add two parameters with command
     H*
     H*  03/29/10 C:190 Project: 25WQD
     H*              1. Bypass the doc/folder edits if dcid = W0070C
     H*              2. Default DEPT & Type Code if remark code = 'CLRWR'
     H*              3. Default Diary type = 'Y' if remark code = 'CLRWR'
     H*              3. Default Diary user = ' ' if remark code = 'CLRWR'
     H*
     H*  02/16/04 C:62  Project: MC144 57208
     H*                 DO NOT CHANGE DESCRIPTION IF VALUE THERE.
     H*
     H*  02/05/04 C:62  Project: PR029
     H*                 COMPILE ONLY
     H*
     H*  12/12/02 C:62  Project: 114MFC
     H*                 Bypass the doc/folder edits if dcid = F0350C
     H*
     H*  04/25/00 C:31  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Replace write to SDCMP with API DA700R
     H*
     H*  11/19/99 C:113 PROJECT: DR079                                *
     H*                 Corrected security flag usage.                *
     H*                                                               *
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
     H*                              DR048/17294                      *
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
     H*                immediately.                                   *
     H*                                                               *
     H*  05/18/99 C:58 Support: 17233, 17594, 16802
     H*                Clear DCID before updating, prevent
     H*                residual data.
     H*
     H*  07/21/98 C:69 Support: 12666
     H*                Changed the way the check for a recursive call
     H*                is done
     H*                                                               *
     H*  02/23/98 C:85 Project: WC577                                 *
     H*                CHANGED THE FIELD MOVED TO CMSMCD              *
     H*                FROM *BLANKS TO CTSMCD.                        *
     H*                                                               *
     H** 05/09/96 C:53  Project: DR016                               **
     H**    1.  CARRY OVER INPUT PARMS TO SCREEN.                    **
     H**                                                             **
     H** 09/23/96 C:53  Project: 25MCP                               **
     H**    1.  CARRY OVER INPUT PARMS TO SCREEN.                    **
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**  03  Office NOT installed: Nondisplay of F1=OV/400          **
     H** N03  Office IS installed:  Display/Use of F1 = OV/400       **
     H**  06  NON DISPLAY ON DOC ID/TYPE/DATE HEADINGS               **
     H**  07  GLOBAL - COMMAND KEY REQUESTED                         **
     H**  21  SCM0531-UNABLE TO PROCESS FOR THIS FOLDER              **
     H**  22  SCM0545-NOT ABLE TO BUILD FOLDER (AUTO-CREATE)         **
     H**  23  SCM0546-NOT ABLE TO BUILD DOCUMENT (AUTO-CREATE)       **
     H**  24  SCM0547-REMARK TYPE MUST BE VALID FOR AUTO-CREATE      **
     H**  25  SCM0548-SET FOLDER/DOC INVALID FOR INCOMING PROGRAM    **
     H**  26  SCM0609-DOCUMENT ID IS NOT ALLOWED WITH A REMARK TYPE  **
     H**        THAT HAS AN ASSOCIATED DATA ENTRY PROGRAM ID.        **
     H**  27  SCM0   -NO SUB-AREA EXISTS FOR THIS REMARK TYPE.       **
     H**  28  SCM0510-INVALID VALUE KEYED                            **
     H**  29  SCM0503-FOLDER REQUIRED                                **RED
     H**  31  GLOBAL - SCM0550-AS/400 Officevision is not installed  **
     H**  32  SCM0532-Default Folder required if Document ID indicat **ed
     H**  33         -ERROR OCCURED ON SCAN OPERATION                **
     H**  34  GLOBAL - DATA ENTRY PROGRAM OPTION IS INSTALLED        **
     H**  35  SCM0502-VALUE MUST BE 'Y' OR 'N'                       **
     H**  37  GLOBAL - Used on call to Data Entry Program (DEPGM)    **
     H**  42  SCM0603-VALUE MUST BE 'Y', 'N', OR BLANK.              **
     H**  43  SCM0604-THIS REMARK TYPE IS NON-PURGEABLE.             **
     H**  44  SCM0605-DIARY TYPE MUST BE 'Y' IN ORDER FOR THIS       **
     H**      REMARK TO BE PURGEABLE.                                **
     H**  46  SCM0502-                                               **
     H**  47  SCM0619-Must find alternative access to this data entry**
     H**             program.                                        **
     H**  48  SCM0620-Data entry program is not available at this    **
     H**             time.                                           **
     H**  51  SCM0501-IF ENTERED, VALUE ENTERED IS NOT DEFINED IN TA **BLE FILE
     H**  52  SCM0500-INVALID DATE ENTERED                           **
     H**  53  SCM0503-VALUE CANNOT BE EQUAL TO BLANKS - REMARK REQUI **RED
     H**  54  SCM0516-IF ENTERED, DOCUMENT NOT FOUND IN SPECIFIED FO **LDER
     H**  55  SCM0512-INVALID REMARK CODE ENTERED                    **
     H**  56  DISPLAY "UNSECURE REMARK", REMARK IS SECURED           **
     H** N56  DISPLAY "SECURE REMARK", REMARK IS UNSECURED           **
     H**  57  SCM0532-FOLDER MUST BE ENTERED IF DOCUMENT IS ENTERED  **
     H**  58  SCM0612-User is not enrolled in DARTS. See your        **
     H**      System Administrator.                                  **
     H**  59  VALIDATE CHAIN COMMAND MATCH FOR DEFAULT SECURITY LVL. **
     H**  60  GLOBAL - GENERAL CHAIN: PSL01, PCM01, PCG01            **EDIT PGM
     H**  71  USER RESTRICTION ON FOLDER                             **
     H**  72  USER RESTRICTION ON DOCUMENT                           **
     H**  73  USER RESTRICTION ON DATE                               **
     H**  74  USER RESTRICTION ON REMARK CODE                        **
     H**  75  USER RESTRICTION ON COMMENT                            **
     H**  76  USER RESTRICTION ON ADDTL REMARKS                      **
     H**  77  USER RESTRICTION ON DIARY TYPE                         **
     H**  78  USER RESTRICTION ON DIARY USER                         **
     H**  91  SCM0525-DOCUMENT NAME MUST BE VALID TO PERFORM ADD FUN **CTION
     H**  92  SCM0517-NOT AUTHORIZED TO DOCUMENT OR FOLDER           **
     H**  93  SCM0518-USER NOT ENROLLED IN OFFICE 400                **
     H**  94  SCM0519-UNABLE TO PROCESS REQUEST FOR THIS DOCUMENT    **
     H**  95  SCM0520-DOCUMENT NAME ALREADY EXISTS AS DOCUMENT OR FO **LDER
     H**  96  SCM0521-FOLDER NOT FOUND                               **
     H**  97  GLOBAL - GENERAL CHAIN: PCM0A                          **
     H** N98  EITHER NO SECURITY OR MEETS SECURITY LEVEL             **
     H**  98  PROTECT REMARK TYPE BECAUSE OF SECURITY LEVEL          **
     H**  99  GLOBAL - IMAGE PRODUCT IS INSTALLED                    **
     H*****************************************************************
     H** FILES:                                                      **
     H**    SDCML (PCM01): KEY/DATE/TIME/SEQ                         **
     H**    SMSLL (PSL01): $$USER                                    **
     H**    SMPPL (PPP01): QQPNAM                                    **
     H**    SMPIL (PPI01): ##PID                                     **
     H**    STCTL1(PCT01): CMRMCD                                    **
     H**    SSYLF (SSY01): XXSYK                                     **
     H**          (SDR01): XXSYK                                     **
     H**    WXERP1(PER01): 7                                         **
     H**    SDCMLI(PCM01): KEY/SEQ                                   **
     H*****************************************************************
     FSC001F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F*** BEGIN DEL *** 04/25/00 **************************************
     F*****SDCML   IF  E           K        DISK                      A
     F*** END DEL *** 04/25/00 ****************************************
     FSMSLL     IF   E           K DISK
     FSMPPL     IF   E           K DISK
     FSMPIL     IF   E           K DISK
     FSTCTL1    IF   E           K DISK
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SP201)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SWC01)
     F*** BEGIN DEL *** 04/25/00 **************************************
     F********SDCMLI  IF  E           K        DISK
     F********    PCM01                             KRENAMEPCM0I
     F*** END DEL *** 04/25/00 ****************************************

     D RF              S              1    DIM(10)                              SCREEN PATH RFMTS
     D CK1             S             70    DIM(7) CTDATA PERRCD(1)              COMMAND KEYS - SCR01
     D*** BEGIN DEL *** 04/25/00 **************************************
     D********PCM0I
     D********      CMKEY                           PIKEY
     D********      CMSEQ#                          PISEQ#
     D*** END DEL *** 04/25/00 ****************************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D                 DS
     D  QX                     1      8
     D                                     DIM(8)                               RESTRICTIONS
     D  QXRFLD                 1      1
     D  QXRDOC                 2      2
     D  QXRDT                  3      3
     D  QXRRMC                 4      4
     D  QXRCOM                 5      5
     D  QXRADD                 6      6
     D  QXRDTY                 7      7
     D  QXRDUS                 8      8
     D                 DS
     D  CI                     1     20
     D                                     DIM(10)                              PATH FILE CSID'S
     D  PICI01                 1      2
     D  PICI02                 3      4
     D  PICI03                 5      6
     D  PICI04                 7      8
     D  PICI05                 9     10
     D  PICI06                11     12
     D  PICI07                13     14
     D  PICI08                15     16
     D  PICI09                17     18
     D  PICI10                19     20
     D*
     D                 DS
     D  FR                     1     10
     D                                     DIM(10)                              PATH FILE RFMTS
     D  PIRF01                 1      1
     D  PIRF02                 2      2
     D  PIRF03                 3      3
     D  PIRF04                 4      4
     D  PIRF05                 5      5
     D  PIRF06                 6      6
     D  PIRF07                 7      7
     D  PIRF08                 8      8
     D  PIRF09                 9      9
     D  PIRF10                10     10
     D*
     D                 DS
     D  PA                     1     20
     D                                     DIM(10)                              SCREEN PATH CSID'S
     D  SCPA01                 1      2
     D  SCPA02                 3      4
     D  SCPA03                 5      6
     D  SCPA04                 7      8
     D  SCPA05                 9     10
     D  SCPA06                11     12
     D  SCPA07                13     14
     D  SCPA08                15     16
     D  SCPA09                17     18
     D  SCPA10                19     20
     D*
     D* FILE INFORMATION DATA STRUCTURE
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D*
     D* PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     *******************************************************************
     C*
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C*** BEGIN CHG *** 04/25/00 **************************************
     C********   *LIKE     DEFN CMCDAT    XXDATE
     C********   *LIKE     DEFN CMTRTM    XXTIME
     C********   *LIKE     DEFN CMSEQ#    XXSEQ#
     C********   *LIKE     DEFN CMCSID    SCCSID
     C     *LIKE         DEFINE    SLCSID        SCCSID
     C     *LIKE         DEFINE    DRAL02        OFCPGM
     C     *LIKE         DEFINE    Q2RECF        HLRECF
     C********   *LIKE     DEFN CMKEY     DEKEY
     C     *LIKE         DEFINE    SLUDSC        DEKEY
     C********   *LIKE     DEFN CMCDAT    XXCDAT
     C     *LIKE         DEFINE    SLDT          XXCDAT
     C********   *LIKE     DEFN CMCDAT    XXDSDT
     C     *LIKE         DEFINE    SLDT          XXDSDT
     C********   *LIKE     DEFN CMCDAT    HHDT
     C*** END CHG *** 04/25/00 ****************************************
     C*** BEGIN DEL *** 04/25/00 **************************************
     C********
     C****KEY*LIST FOR THE REMARKS DETAIL RECORD
     C********
     C********   CMKYA     KLIST
     C********             KFLD           QQKEY
     C********             KFLD           ##SEQ#
     C********
     C****KEY*LIST FOR THE REMARKS DETAIL RECORD
     C********
     C********   CMKY1     KLIST
     C********             KFLD           QQKEY
     C********             KFLD           XXDATE
     C********             KFLD           XXTIME
     C********             KFLD           XXSEQ#
     C*** END DEL *** 04/25/00 ****************************************
     C*
     C*   KEY LIST FOR THE SECURITY FILE
     C*
     C     SLKY          KLIST
     C                   KFLD                    $$USER
     C*
     C*   KEY LIST FOR THE PROGRAM/PATH FILE
     C*
     C     PPKY          KLIST
     C                   KFLD                    QQPNAM
     C*
     C*   KEY LIST FOR THE PATH ID
     C*
     C     PIKY          KLIST
     C                   KFLD                    ##PID             8
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*** BEGIN DEL *** 04/25/00 **************************************
     C********
     C****KEY*LIST FOR THE REMARK TYPE FILE
     C********
     C********   CTKY      KLIST
     C********             KFLD           CMRMCD
     C*** END DEL *** 04/25/00 ****************************************
     C*
     C     CTKY1         KLIST
     C                   KFLD                    SCCODE            5
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQKEY            40
     C                   PARM                    QQPNAM           10
     C                   PARM                    QQFOLD           80
     C                   PARM                    QQDCID           12
     C                   PARM                    QQDT              8
     C                   PARM                    QQRMCD            5
     C                   PARM                    QQCOM            60
     C                   PARM                    QQDUSR           10
     C     QXRFLD        PARM                    QQRFLD            1
     C     QXRDOC        PARM                    QQRDOC            1
     C     QXRDT         PARM                    QQRDT             1
     C     QXRRMC        PARM                    QQRRMC            1
     C     QXRCOM        PARM                    QQRCOM            1
     C     QXRADD        PARM                    QQRADD            1
     C     QXRDTY        PARM                    QQRDTY            1
     C     QXRDUS        PARM                    QQRDUS            1
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
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVEL     'SC'          XXSYK
     C*
     C*** Beg Add *** 12/19/12 **************************************
      /Free
       AV_KEYS = %subst(ck1(2):1:55)  + '   ' + %subst(ck1(3):1:55) +
        '   ' + %subst(ck1(5):1:55) + '   ' + %subst(ck1(6):15:12)   +
        '   ' + 'F23=Secure/Unsecure Remark Code';
      /End-Free
     C*** End Add *** 12/19/12 **************************************
     C****    INITIALIZE ROUTINE IS PREFORM ONCE   ******
     C                   EXSR      INZSR
     C*
     C*                    *** DISPLAY SCREEN(S)
     C*------------------------*
     C     STRT          TAG
     C*------------------------*
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF
     C*
     C     Q2RECF        IFEQ      'SCR04'
     C                   EXFMT     SCR04
     C                   ENDIF
     C*
     C*                    -------------------------------*
     C*                    *** F7 PROCESS ALL COMMAND KEYS
     C*                    -------------------------------*
     C     *IN07         IFEQ      '1'
     C     *INKA         IFEQ      '1'
     C*                    ----------------*
     C*                    *** F1 = OV/400
     C*                    ----------------*
      *** BEGIN CHG *** 11/19/99 ***********************************************
     C********** SLF03     IFEQ 'Y'
     C     SLF01         IFEQ      'Y'
      *** END   CHG *** 11/19/99 ***********************************************
     C                   MOVE      *BLANKS       QQRTCD
     C                   MOVE      *BLANKS       OFCPGM
     C                   MOVEL     Q2RECF        HLRECF
     C*                    ----------------------------------------
     C*                    Get Office CL Pgm Name from SDRPF/DRAL02
     C*                    ----------------------------------------
     C     SYKY          CHAIN     SDR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     DRAL02        OFCPGM
     C                   ENDIF
     C*
     C                   CALL      OFCPGM
     C                   PARM                    QQRTCD
     C*
     C                   WRITE     SCR99
     C                   MOVEL     HLRECF        Q2RECF
     C                   ENDIF                                                  END SLF01=Y
     C                   GOTO      STRT
     C                   ENDIF                                                  END *INKA
     C*
     C*                    -------------*
     C*                    *** F3 = EXIT
     C*                    -------------*
     C     *INKC         IFEQ      '1'
     C     SLF03         IFEQ      'Y'
     C     Q2RECF        OREQ      'SCR04'                                      12/18/91 ADD
     C                   GOTO      TERM
     C                   ENDIF                                                  END SLF03=Y
     C                   GOTO      TERM
     C                   ENDIF                                                  END *INKC
     C*
     C*                    -------------------------*
     C*                    *** F5 = DISPLAY DOCUMENT
     C*                    -------------------------*
     C     *INKE         IFEQ      '1'
     C     ADDDOC        TAG
     C     SLF05         IFEQ      'Y'
     C     Q2RECF        IFEQ      'SCR01'
     C*                    --------------------------------------------
     C*                    AS/400 office not install - disallow OV/400
     C*                    --------------------------------------------
     C     XXOFIF        IFEQ      'N'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C* * * ADD DOCUMENT
     C*
     C                   MOVEA     '0000'        *IN(51)
     C                   MOVEA     '0000'        *IN(56)
     C                   MOVEA     '000000'      *IN(91)
     C*
     C     SLADC         IFNE      'Y'
     C                   MOVE      '1'           *IN57                          ADD NOT ALLOWED
     C                   GOTO      STRT
     C                   END
     C*
     C*** DEFAULT FOLDER REQUIRED
     C     SCDFLR        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C*
     C*** VALIDATE FOLDER:
     C                   MOVEL     SCDFLR        XXFOLD           80            DEFAULT FOLDER
     C                   MOVE      *BLANK        QQRTCD            1
     C                   CALL      'SVALFLR'
     C                   PARM                    XXFOLD
     C                   PARM                    QQRTCD
     C*** THAT IT EXISTS:
     C     QQRTCD        IFEQ      '1'
     C     SCDCID        ANDNE     'F0269C1'
     C*** BEGIN ADD *** 03/29/10 **************************************
     C     SCDCID        ANDNE     'W0070C'
     C*** END   ADD *** 03/29/10 **************************************
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*** THAT USER HAS AUTHORITY TO IT
     C     QQRTCD        IFEQ      '2'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS RETURN SELECTED FOLDER PATH
     C*
     C                   MOVEL     SCDFLR        QQDFLR
     C*
     C     SCDCID        CABEQ     *BLANK        STRT                     91
     C*
     C                   MOVEL     SCDCID        XXDCID
     C                   MOVEL     SCDFLR        XXFOLD           80
     C                   MOVEL     *BLANK        QQRTCD            1
     C                   CALL      'SADDDOC'
     C                   PARM                    XXDCID           12
     C                   PARM                    XXFOLD
     C                   PARM                    QQRTCD
     C                   PARM                    QQMS              1
     C*** Begin Add *** 10/17/11 **************************************
     C                   PARM      *BLANK        XXUNCN          255
     C                   PARM      *BLANK        XXFSF             1
     C*** End   Add *** 10/17/11 **************************************
     C*
     C                   SELECT
     C     QQRTCD        WHENEQ    '2'
     C                   MOVE      '1'           *IN92
     C     QQRTCD        WHENEQ    '3'
     C                   MOVE      '1'           *IN93
     C     QQRTCD        WHENEQ    '4'
     C                   MOVE      '1'           *IN94
     C     QQRTCD        WHENEQ    '5'
     C                   MOVE      '1'           *IN95
     C     QQRTCD        WHENEQ    '6'
     C                   MOVE      '1'           *IN96
     C                   GOTO      STRT
     C                   ENDSL
     C                   ENDIF                                                  SCR01
     C                   ENDIF                                                  END SLF05=Y
     C                   GOTO      STRT
     C                   ENDIF                                                  END *INKE
     C*
     C*                    ------------------------------------*
     C*                    *** F6 = SEARCH DOCUMENT IN A FOLDER
     C*                    ------------------------------------*
     C     *INKF         IFEQ      '1'
     C     SLF06         IFEQ      'Y'
     C*                    DISPLAY DOCUMENTS IN A FOLDER
     C     Q2RECF        IFEQ      'SCR01'
     C*                    --------------------------------------------
     C*                    AS/400 office not install - disallow OV/400
     C*                    --------------------------------------------
     C     XXOFIF        IFEQ      'N'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C* * * DSP DOCUMENTS
     C*
     C                   MOVEA     '0000'        *IN(51)
     C                   MOVEA     '0000'        *IN(56)                          SETOF ERR INDS
     C                   MOVEA     '000000'      *IN(91)
     C                   MOVEL     *BLANK        XXDCID                         RETURN DOC ID
     C                   MOVEL     SCDFLR        XXFOLD           80            DEFAULT FOLDER
     C                   MOVEL     *BLANK        QQRTCD            1            ERR RTN CODE
     C*
     C                   CALL      'SDSPFLR'                                    "DSPFLR" OFC API
     C                   PARM                    XXFOLD                         DFLT FOLDER
     C                   PARM                    XXDCID           12            RTN DOC ID SLCT
     C                   PARM                    QQRTCD                         ERR ID CODE
     C*
     C                   SELECT
     C     QQRTCD        WHENEQ    '1'
     C                   MOVE      '1'           *IN91
     C     QQRTCD        WHENEQ    '2'
     C                   MOVE      '1'           *IN92
     C     QQRTCD        WHENEQ    '3'
     C                   MOVE      '1'           *IN93
     C     QQRTCD        WHENEQ    '4'
     C     SCDCID        ANDNE     'F0269C1'
     C*** BEGIN ADD *** 03/29/10 **************************************
     C     SCDCID        ANDNE     'W0070C'
     C*** END   ADD *** 03/29/10 **************************************
     C                   MOVE      '1'           *IN21
     C     QQRTCD        WHENEQ    '5'
     C                   MOVE      '1'           *IN95
     C     QQRTCD        WHENEQ    '6'
     C                   MOVE      '1'           *IN96
     C                   GOTO      STRT
     C                   ENDSL
     C*
     C     XXDCID        IFNE      *BLANKS                                      VALID DOC ID
     C                   MOVEL     XXDCID        SCDCID                         USE IT, GOTO
     C                   GOTO      STRT                                         START
     C                   ENDIF
     C                   ENDIF                                                  SCR01
     C                   ENDIF                                                  END SLF06=Y
     C                   GOTO      STRT
     C                   ENDIF                                                  END *INKF
     C*
     C*                    -----------------------------*
     C*                    *** F9 = USER PROFILE SEARCH
     C*                    -----------------------------*
     C     *INKI         IFEQ      '1'
     C     SLF09         IFEQ      'Y'
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'SC530R'
     C                   PARM                    SCDUSR           10
     C                   PARM                    QQRTCD            1
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  END SCR01
     C                   ENDIF                                                  END SLF09=Y
     C                   GOTO      STRT
     C                   ENDIF                                                  END *INKI
     C*
     C*                    ---------------------------*
     C*                    *** F10 = TABLE FILE SEARCH
     C*                    ---------------------------*
     C     *INKJ         IFEQ      '1'
     C     SLF10         IFEQ      'Y'
     C     Q2RECF        IFEQ      'SCR01'
     C*** BEGIN CHG *** 03/29/10 **************************************
     C     SCDEPT        IFEQ      '  '
     C******     SCDEPT    IFEQ '? '
     C*** END   CHG *** 03/29/10 **************************************
     C                   MOVEL     *BLANKS       SCCODE
     C                   MOVE      *BLANK        QQRTCD
     C                   CALL      'SC580R'
     C                   PARM                    SCCODE
     C                   PARM                    QQRTCD
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVEL     SCCODE        SCDEPT
     C                   MOVE      SCCODE        SCRMTY
     C     CTKY1         CHAIN     PCT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CTFL01        SCDITP
     C                   ENDIF
     C                   GOTO      STRT
     C                   ENDIF                                                  SCCODE ='? '
     C*
     C*** BEGIN CHG *** 03/29/10 **************************************
     C     SCRMTY        IFEQ      '  '
     C*********  SCRMTY    IFEQ '? '
     C*** END   CHG *** 03/29/10 **************************************
     C                   MOVEL     *BLANKS       SCCODE
     C                   MOVEL     SCDEPT        SCCODE
     C                   MOVE      *BLANK        QQRTCD
     C                   CALL      'SC580R'
     C                   PARM                    SCCODE
     C                   PARM                    QQRTCD
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVEL     SCCODE        SCDEPT
     C                   MOVE      SCCODE        SCRMTY
     C     CTKY1         CHAIN     PCT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CTFL01        SCDITP
     C                   ENDIF
     C                   GOTO      STRT
     C                   ENDIF                                                  SCCODE ='? '
     C                   ENDIF                                                  END SCR01
     C                   ENDIF                                                  END SLF10=Y
     C                   GOTO      STRT
     C                   ENDIF                                                  END *INKJ
     C*
     C*                    ------------------------------------------*
     C*                    *** F20 = SET DEFAULT FOLDER & DOCUMENT ID
     C*                    ------------------------------------------*
     C     *INKU         IFEQ      '1'
     C*                         VALIDATE REMARK TYPE
     C                   MOVEL     SCDEPT        SCCODE
     C                   MOVE      SCRMTY        SCCODE
     C*
     C*                         REMARK TYPE MUST BE ENTERED
     C     SCCODE        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   ELSE
     C     SCCODE        CHAIN     STCTL1                             24
     C     *IN24         CABEQ     '1'           STRT
     C     CTTYPE        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       SCDFLR
     C                   MOVE      *BLANKS       SCDCID
     C                   MOVE      *BLANKS       QQCODE
     C                   MOVEL     CTTYPE        QQCODE
     C                   MOVE      *BLANKS       QQDFLR
     C                   MOVE      *BLANKS       QQDCID
     C                   MOVE      *BLANKS       QQERR
     C*** Begin Chg *** 04/17/15 *****************************************
     C**********         CALL      'SCEXITC'                            99
     C                   CALL      'SCEXITC'                            90
     C*** End   Chg *** 04/17/15 *****************************************
     C                   PARM                    QQKEY
     C                   PARM                    QQPNAM
     C                   PARM                    QQCODE            2
     C                   PARM                    QQDFLR           40
     C                   PARM                    QQDCID           12
     C                   PARM                    QQERR             1
     C     QQERR         IFNE      *BLANKS
     C     QQERR         IFEQ      '1'
     C*                         INCOMING PROGRAM DOES NOT ALLOW
     C*                         AUTO CREATE FUNCTION...
     C                   MOVE      '1'           *IN25
     C                   ENDIF                                                  QQERR=1
     C     QQERR         IFEQ      '2'
     C*                         NOT ABLE TO AUTO BUILD FOLDER
     C                   MOVE      '1'           *IN22
     C                   ENDIF                                                  QQERR=2
     C     QQERR         IFEQ      '3'
     C*                         NOT ABLE TO AUTO BUILD DOCUMENT ID
     C                   MOVE      '1'           *IN23
     C                   ENDIF                                                  QQERR=3
     C                   GOTO      STRT
     C                   ENDIF                                                  QQERR<>" "
     C                   MOVE      *BLANKS       SCDFLR
     C                   MOVE      *BLANKS       SCDCID
     C                   MOVEL     QQDFLR        SCDFLR
     C                   MOVEL     QQDCID        SCDCID
     C                   GOTO      ADDDOC
     C                   ENDIF                                                  END *INKU
     C*
     C*                    ---------------------------------*
     C*                    *** F23 = SECURE REMARKS BY LEVEL
     C*                    ---------------------------------*
     C     *INKX         IFEQ      '1'
     C     SLF23         IFEQ      'Y'
     C*                         Remark UNSECURED to SECURED
     C     Q2RECF        IFEQ      'SCR01'
     C     *IN56         IFEQ      '0'
     C                   MOVEL     *BLANKS       SCCODE
     C                   MOVEL     SCDEPT        SCCODE
     C                   MOVE      SCRMTY        SCCODE
     C     CTKY1         CHAIN     PCT01                              59        +DEFAULT SECLVL
     C     *IN59         IFEQ      '1'
     C     CTDSLV        OREQ      *ZEROS
     C                   MOVE      SLSECL        SCSECL
     C                   ELSE
     C                   MOVE      CTDSLV        SCSECL
     C                   ENDIF
     C                   MOVE      '1'           *IN56
     C                   MOVE      SCSECL        ##SECL
     C                   MOVE      'Y'           ##SBYL            1
     C*                         DISPLAY "UNSECURE REMARKS", IF
     C*                           "SECURE REMARKS" WAS DISPLAYED....
     C     C1            IFEQ      6
     C                   Z-ADD     7             C1
     C                   ENDIF
     C                   ELSE
     C*                         Remark SECURED to UNSECURED
     C                   MOVE      '0'           *IN56
     C                   MOVE      *ZEROS        SCSECL
     C                   MOVE      SCSECL        ##SECL
     C                   MOVE      'N'           ##SBYL            1
     C*                         DISPLAY "SECURE REMARKS", IF
     C*                           "UNSECURE REMARKS" WAS DISPLAYED....
     C     C1            IFEQ      7
     C                   Z-ADD     6             C1
     C                   ENDIF
     C                   ENDIF
     C*** Beg Del *** 12/19/12 **************************************
     C*****              MOVEL     CK1(C1)       SCCML1
     C*** End Chg *** 12/19/12 **************************************
     C                   ENDIF
     C                   ENDIF                                                  END SLF23=Y
     C                   GOTO      STRT
     C                   ENDIF                                                  END *INKX
     C*
     C     *INKY         IFEQ      '1'
     C*                    -------------------*
     C*                    *** F24 = More Keys
     C*                    -------------------*
     C     SLF24         IFEQ      'Y'
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C*                    AS/400 office not install, Image not install
     C*                       Remark Secured....
     C     *IN03         IFEQ      '1'
     C     *IN56         ANDEQ     '1'
     C     C1            IFEQ      1
     C                   Z-ADD     3             C1
     C                   ELSE
     C     C1            IFEQ      3
     C                   Z-ADD     4             C1
     C                   ELSE
     C     C1            IFEQ      4
     C                   Z-ADD     7             C1
     C                   ELSE
     C     C1            IFEQ      7
     C                   Z-ADD     1             C1
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C*                    AS/400 office not install, Image not install
     C*                       Remark Not Secured....
     C     *IN03         IFEQ      '1'
     C     *IN56         ANDEQ     '0'
     C     C1            IFEQ      1
     C                   Z-ADD     3             C1
     C                   ELSE
     C     C1            IFEQ      3
     C                   Z-ADD     4             C1
     C                   ELSE
     C     C1            IFEQ      4
     C                   Z-ADD     6             C1
     C                   ELSE
     C     C1            IFEQ      6
     C                   Z-ADD     1             C1
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C*                    AS/400 office install, Image not install,
     C*                       Remark Secured....
     C     *IN03         IFEQ      '0'
     C     *IN56         ANDEQ     '1'
     C     C1            IFEQ      2
     C                   Z-ADD     3             C1
     C                   ELSE
     C     C1            IFEQ      3
     C                   Z-ADD     4             C1
     C                   ELSE
     C     C1            IFEQ      4
     C                   Z-ADD     7             C1
     C                   ELSE
     C     C1            IFEQ      7
     C                   Z-ADD     2             C1
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C*                    AS/400 office install, Image not install,
     C*                       Remark Not Secured....
     C     *IN03         IFEQ      '0'
     C     *IN56         ANDEQ     '0'
     C     C1            IFEQ      2
     C                   Z-ADD     3             C1
     C                   ELSE
     C     C1            IFEQ      3
     C                   Z-ADD     4             C1
     C                   ELSE
     C     C1            IFEQ      4
     C                   Z-ADD     6             C1
     C                   ELSE
     C     C1            IFEQ      6
     C                   Z-ADD     2             C1
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*** Beg Del *** 12/19/12 **************************************
     C*****              MOVEL     CK1(C1)       SCCML1
     C*** End Del *** 12/19/12 **************************************
     C                   ENDIF                                                  END SCR01
     C                   ENDIF                                                  END SLF24=Y
     C                   GOTO      STRT
     C                   ENDIF                                                  END *INKY
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN11/CMDKEYS
     C*
     C*****************************************************************
     C*   GOTO THE APPROPRIATE SCREEN
     C*****************************************************************
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR04'       SCR04T
     C*
     C*   PROCESS REQUESTED SCREEN
     C*
     C*------------------------*
     C     SCR01T        TAG
     C*------------------------*
     C*
     C                   EXSR      EDTS01                                        EDIT SCR01 FIELDS
     C*
     C* WRITE REMARK RECORD
     C                   EXSR      WCOM
     C*
     C* EXIT PROGRAM
     C                   GOTO      TERM
     C*
     C*----------------*
     C     SCR04T        TAG
     C*----------------*
     C                   GOTO      STRT
     C*
     C*----------------*
     C     TERM          TAG
     C*----------------*
     C*
     C                   MOVE      '1'           *INLR
     C*
     C*======================================================          ====*
     C*              INIT SUBROUTINE                        *          T ***
     C*======================================================          ====*
     CSR   INZSR         BEGSR
     C*** BEGIN DEL *** 04/25/00 **************************************
     C***99N99   CMKY1     SETLLPCM01
     C*** END DEL *** 04/25/00 ****************************************
     C*                                                                ====*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCDAT
     C*
     C*** BEGIN DEL *** 04/25/00 **************************************
     C********             Z-ADD*ZEROS    XXSEQ#
     C********             Z-ADD*HIVAL    ##SEQ#  50
     C*** END DEL *** 04/25/00 ****************************************
     C                   Z-ADD     *ZEROS        ##SECL            3 0
     C                   MOVE      'N'           ##SBYL
     C*
     C                   Z-ADD     *ZEROS        C1                5 0
     C                   MOVE      'N'           XXDEWR            1
     C                   MOVE      *BLANKS       DERTCD            1
     C                   MOVE      *BLANKS       DECOM1          256
     C                   MOVE      *BLANKS       DECOM2           19
     C                   MOVE      *BLANKS       DEKEY
     C                   MOVE      *BLANKS       SCRFMT            1
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   MOVEL     QQPNAM        SCPNAM
     C                   MOVEL     QQPRDS        SCPRDS
     C                   MOVEL     QQFOLD        SCDFLR
     C                   MOVE      QQDCID        SCDCID
     C                   Z-ADD     71            XX                2 0
     C*
     C* CHECK RESTRICTION FLAGS
     C*
     C     1             DO        8             X                 1 0
     C     QX(X)         IFEQ      'Y'
     C                   MOVE      '1'           *IN(XX)
     C                   ELSE
     C                   MOVE      '0'           *IN(XX)
     C                   ENDIF
     C                   ADD       1             XX
     C                   ENDDO
     C*
     C* BUILD KEY DISPLAY HERE
     C*
     C                   MOVEL     QQKEY         X#KEY                    99     = ON IF BLANKS
     C                   MOVE      ' '           X#RTCD
     C                   MOVEL     QQPNAM        X#PNAM
     C  N99              CALL      'SRKCVT'
     C                   PARM                    X#PNAM           10
     C                   PARM                    X#KEY            79
     C                   PARM                    X#RTCD            1
     C*
     C     X#RTCD        IFNE      'N'
     C                   MOVEL     X#KEY         SCKEY
     C                   ELSE
     C                   MOVEL     QQKEY         SCKEY
     C                   END
     C*
     C     SLCCIF        IFNE      'Y'
     C                   MOVE      '1'           *IN98
     C                   ELSE
     C                   MOVE      '0'           *IN98
     C                   ENDIF
     C*
     C*                      Check System Control File Install Flags
     C*                           for this Client.....
     C     'SY'          CHAIN     SSY01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*                      Check for AS/400 Office Installed
     C     SYOFIF        IFEQ      'Y'
     C                   MOVE      'Y'           XXOFIF            1            Installed
     C                   MOVE      '0'           *IN03                          ADD 04/06/92
     C                   ELSE                                                   ELS 'Y' SYOFIF
     C                   MOVE      'N'           XXOFIF                         Installed
     C                   MOVE      '1'           *IN03                          ADD 04/06/92
     C                   END                                                    END 'Y' SYOFIF
     C                   END                                                    END 0 *IN60
     C*
     C*                      Look at Darts System Control File to
     C*                           Determine what's Installed....
     C     SYKY          CHAIN     SDR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*                      If Data Entry Program Option is Installed,
     C*                           Set On Indicator.
     C*                           (DRFL03 = 'Y', *IN34 = 1)
     C     DRFL03        IFEQ      'Y'
     C                   MOVE      '1'           *IN34
     C                   END                                                    ADD 04/09/92
     C*
     C*                      If Summary Code Option is Installed,
     C*                           Set On Indicator....
     C*                             (DRFL02 = 'Y', *IN36 = 1)
     C*                             INITIALIZE SCSMCD TO *BLANKS
     C     DRFL02        IFEQ      'Y'
     C                   MOVE      '1'           *IN36
     C                   MOVE      *BLANKS       SCSMCD            4
     C                   ENDIF
     C                   ENDIF                                                  END 0 *IN60
     C*
     C                   MOVEL     SLCSID        SCCSID
     C*
     C*                      GET INITIAL PATH INFORMATION
     C     PPKY          CHAIN     PPP01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*                      ALLOW OUTSIDE RANGE IF 'Y' IN PATH
     C*                           RECORD, AND SECURITY FILE...
     C                   MOVEL     PPPATH        ##PID
     C                   EXSR      PIGET
     C*
     C     PIALOR        IFEQ      'Y'
     C     SLCCIF        ANDEQ     'Y'
     C                   MOVE      '0'           *IN98
     C                   ELSE
     C                   MOVE      '1'           *IN98
     C                   END
     C                   ELSE
     C                   MOVEL     SCCSID        PA(1)
     C                   MOVEL     SCRFMT        RF(1)
     C                   END
     C*
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      'SCR01'       SCRECF                          FOR DISPLAY
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C*
     C*                       ADD REMARK
     C                   MOVEL     Q2RECF        $$RECF           10
     C                   MOVEL     *BLANK        SCCOMM
     C*
     C                   MOVEL     'N'           SCARMK                             SCREEN FLAG
     C*
     C*                      DEFAULT TO CURRENT USER FOR DIARY USER,
     C*                           BUT USER MAY CHANGE TO ANOTHER
    >C     QQDUSR        IFNE      *BLANKS
     C                   MOVEL     QQDUSR        SCDUSR
     C                   ELSE
     C                   MOVEL     Q1USER        SCDUSR
     C                   ENDIF
     C*** BEGIN CHG *** 03/29/10 **************************************
     C     QQRMCD        IFEQ      'CLRWR'
     C*** BEGIN ADD *** 05/31/11 **************************************
     C     QQRMCD        OREQ      'CLWCD'
     C*** END   ADD *** 05/31/11 **************************************
     C                   MOVEL     *BLANKS       SCDUSR
     C                   ENDIF
     C*** END   CHG *** 03/29/10 **************************************
     C*
     C*                      CHAIN TO SECURITY FILE FOR DEFAULTS
     C*                           AND AUTHORIZATION
     C                   MOVEL     Q1USER        $$USER           10
     C     SLKY          CHAIN     PSL01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVEL     *BLANKS       $$USER           10
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*                      CHECK PASSED-IN REMARK CODE, IF NOT
     C*                           BLANKS GET DESC.
    >C     QQRMCD        IFNE      *BLANKS
     C     QQRMCD        CHAIN     PCT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CTDESC        SCDESC                   06     '0' = DSP
     C                   MOVEL     QQRMCD        SCDEPT
     C                   MOVE      QQRMCD        SCRMTY
     C*                      CHECK PASSED-IN DEFAULT REMARK IF NOT
     C*                           MOVE INFORMATION TO COMMENT AREA
     C     QQCOM         IFNE      *BLANKS
     C                   MOVEL     QQCOM         SCCOMM
     C                   ENDIF
     C                   ELSE
     C                   MOVE      *BLANKS       SCDESC                   06     '1' = NDSP
     C                   ENDIF                                                     60 = '0'
     C                   ENDIF                                                    QQRMCD = BLANKS
     C*** BEGIN ADD *** 03/29/10 **************************************
     C     QQRMCD        IFEQ      'CLRWR'
     C*** BEGIN ADD *** 05/31/11 **************************************
     C     QQRMCD        OREQ      'CLWCD'
     C*** END   ADD *** 05/31/11 **************************************
     C                   MOVEL     SCDESC        SCCOMM
     C                   ENDIF
     C*** BEGIN ADD *** 03/29/10 **************************************
     C*
     C*   Initialize flag for checking to see if Data Entry Program
     C*     has been called once already.
     C*
     C                   MOVE      'N'           XXDEFL            1
     C                   MOVEL     *BLANKS       SCCODE
     C   06SLRMCD        IFNE      *BLANKS
     C                   MOVEL     SLRMCD        SCDEPT
     C                   MOVE      SLRMCD        SCRMTY
     C                   MOVEL     SLRMCD        SCCODE
     C                   ELSE
     C                   MOVEL     SLCODE        SCDEPT
     C                   MOVEL     SLCODE        SCCODE
     C                   ENDIF
     C*                                                    03/20/92
     C*    INITIALIZE 'PURGEABLE ITEM' FIELD TO BLANK
     C*
     C                   MOVE      *BLANK        SCPGFL
     C     QQRDTY        IFNE      *BLANKS
     C                   MOVE      QQRDTY        SCDITP
     C                   ELSE
     C                   MOVE      ' '           SCDITP
     C*
     C*                      IF ENTERED, REMARK TYPE MUST BE VALID
     C     SCCODE        IFNE      *BLANKS
     C     SCCODE        CHAIN     STCTL1                             60
     C*
     C*                      DETERMINE IF REMARK TYPE IS DIARY OR NOT
     C     CTFL01        IFEQ      'Y'
     C                   MOVE      'Y'           SCDITP
     C                   ELSE
     C                   MOVE      'N'           SCDITP
     C                   ENDIF
     C                   ENDIF                                                   SCCODE NE *BLANKS
     C                   ENDIF
     C*
     C     SLCCTF        IFNE      'Y'
     C                   MOVE      '1'           *IN98                          PROTECT REMARK TYPE
     C                   ELSE
     C                   MOVE      '0'           *IN98
     C                   ENDIF
     C*** BEGIN ADD *** 03/29/10 **************************************
     C                   MOVEL     SCDEPT        SCCODE
     C                   MOVE      SCRMTY        SCCODE
     C     SCCODE        CHAIN     STCTL1                             60
     C     CTFL01        IFEQ      'Y'
     C                   MOVE      'Y'           SCDITP
     C                   ELSE
     C                   MOVE      'N'           SCDITP
     C                   ENDIF
     C*** END   ADD *** 03/29/10 **************************************
     C*
     C*                      If OV/400 is installed
     C     *IN03         IFEQ      '0'
     C                   Z-ADD     2             C1                                         RK TYPE
     C*                                                                ====*
     C*                      If OV/400 is NOT installed
     C                   ELSE
     C                   Z-ADD     1             C1
     C                   ENDIF
     C*** Beg Del *** 12/19/12 **************************************
     C*****              MOVEL     CK1(C1)       SCCML1
     C*** End Del *** 12/19/12 **************************************
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     CSR                 ENDSR
     C*====================================================================*
     C* * *   PIGET SUBROUTINE (PATH ID)                       H D G E T ***
     C*====================================================================*
     CSR   PIGET         BEGSR
     C     PIKY          CHAIN     PPI01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CI            PA
     C                   MOVEL     FR            RF
     C                   Z-ADD     1             #P                5 0
     C                   MOVEA     '00000000'    *IN(11)
     C                   MOVEA     '00'          *IN(19)
     C                   MOVEA     '00000000'    *IN(61)
     C                   MOVEA     '00'          *IN(69)
     C                   MOVE      '1'           *IN61
     C                   MOVE      PA(#P)        SCCSID
     C                   MOVE      RF(#P)        SCRFMT
     C                   ENDIF
     CSR                 ENDSR
     C*====================================================================*
     C* * *   WCOM  SUBROUTINE - WRITE REMARK RECORD           W C O M   ***
     C*====================================================================*
     CSR   WCOM          BEGSR
     C*** BEGIN DEL *** 04/25/00 **************************************
     C********             TIME           DSDTTM
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     HHDT
     C********
     C********             Z-ADDDSTIME    HHTIME  60
     C********
     C****GET*LAST SEQ# USED
     C********
     C******WRITE REMARKS  TO FILE * * *
     C********
     C********             MOVE ##SECL    CMSECL
     C********             MOVE ##SBYL    CMSBYL
     C***99***             MOVELSCKEY     QQKEY
     C***99***             MOVELSCKEY     CMKEY
     C**N99***             MOVELQQKEY     CMKEY
     C**N99***             MOVELQQKEY     SCKEY
     C********             MOVELSCCSID    CMCSID           ANY LONGER
     C********             MOVELQQPNAM    CMPNAM
     C********             MOVE *BLANKS   CMCODE
     C********             MOVELSCCODE    CMRMCD
     C********                                                         *********
     C****BEGIN ADD *** 02/23/98 ***********************************************
     C********                                                         *********
     C********   CMRMCD    CHAINPCT01                60
     C********                                                         *********
     C********   *IN60     IFEQ *OFF
     C********             MOVE CTSMCD    CMSMCD
     C********             ENDIF                           *IN60=OFF
     C********                                                         *********
     C****END*  ADD *** 02/23/98 ***********************************************
     C********
     C****BEGIN ADD *** 05/18/99 ***********************************************
     C********             CLEARCMDCID
     C****END*  ADD *** 05/18/99 ***********************************************
     C********   XXDEWR    IFEQ 'Y'
     C********             MOVELCTDEPG    CMDCID
     C********             MOVE *BLANKS   CMCOMM
     C********             MOVELDECOM1    CMCOMM
     C********             MOVE DECOM2    CMCOMM
     C********             MOVELDEKEY     CMFOLD
     C********             MOVEL'Y'       CMFL01
     C********             MOVEL'N'       XXDEWR
     C********             ELSE
     C********             MOVELSCDCID    CMDCID
     C********             MOVELSCCOMM    CMCOMM
     C********             MOVE ' '       CMFL01
     C********             MOVELSCDFLR    CMFOLD
     C********             END
     C********
     C********             Z-ADDSCCDAT    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     CMCDAT
     C********
     C********             Z-ADDHHDT      CMTRDT
     C********             Z-ADDHHTIME    CMTRTM
     C********
     C********               IF DIARY USER ENTERED, WRITE TO CMUSER
     C********   SCDUSR    IFNE *BLANKS
     C********             MOVELSCDUSR    CMUSER
     C********             ELSE                            ELSE NE *BLANKS
     C********               ELSE DEFAULT TO CURRENT USER
     C********             MOVELQ1USER    CMUSER
     C********             END                             END NE *BLANKS
     C********
     C********             MOVELQ1USER    CMIUSR
     C********             TIME           DSDTTM
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     CMDT
     C********
     C********             Z-ADDDSTIME    CMTIME
     C********   SCDITP    IFEQ 'Y'
     C********   SCDITP    OREQ 'N'
     C********
     C********             MOVE SCDITP    CMDIFL
     C********             ELSE
     C********             MOVE ' '       CMDIFL
     C********             END
     C********                                             DFT LOC=DASD
     C********             MOVEL'D'       CMFL05           ADD 05/11/93
     C********
     C********             MOVE SCPGFL    CMFL02           ADD 03/20/92
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     CMIDT
     C********
     C********             Z-ADD*ZERO     CMDCDT
     C********             MOVELQ1PGM     CMIPGM
     C********             MOVELQ1PGM     CMPGM
     C********                                                         *********
     C****BEGIN DEL *** 02/23/98 ***********************************************
     C**********           MOVE *BLANKS   CMSMCD
     C****END*  DEL *** 02/23/98 ***********************************************
     C********                                                         *********
     C********             MOVE *BLANKS   CMFL04
     C********             MOVE *BLANKS   CMFL05
     C********
     C****GET*LAST SEQ# USED
     C********
     C********   CMKYA     SETGTPCM0I
     C********             READPPCM0I                    60
     C********   *IN60     IFEQ '0'
     C********   SCKEY     ANDEQPIKEY
     C********   PISEQ#    ADD  1         CMSEQ#
     C********             ELSE
     C********             Z-ADD1         CMSEQ#
     C********             END
     C********
     C********             WRITEPCM01
     C******BEGIN ADD **** 07/01/99 ***********************************
     C********             FEOD SDCML
     C******END   ADD **** 07/01/99 ***********************************
     C*** END DEL *** 04/25/00 ****************************************
     C*** BEGIN ADD *** 04/25/00 **************************************
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C   99              MOVEL     SCKEY         QQKEY
     C   99              MOVEL(P)  SCKEY         Q#KEY            40
     C  N99              MOVEL     QQKEY         Q#KEY
     C  N99              MOVEL     QQKEY         SCKEY
     C                   MOVEL(P)  SCCODE        Q#RMCD            5
     C     XXDEWR        IFEQ      'Y'
     C     Q#RMCD        CHAIN     PCT01                              60
     C   60              MOVEL(P)  *BLANKS       Q#DCID           12
     C  N60              MOVEL     CTDEPG        Q#DCID
     C                   MOVEL(P)  DECOM1        Q#COMM          256
     C                   MOVEL(P)  DECOM2        Q#COM2           19
     C                   MOVEL(P)  DEKEY         Q#FOLD           80
     C                   MOVEL     'Y'           Q#FL01            1
     C                   MOVEL     'N'           XXDEWR
     C                   ELSE                                                   DEWR = 'Y'
     C                   MOVEL(P)  SCDCID        Q#DCID
     C                   MOVEL(P)  SCCOMM        Q#COMM
     C                   MOVE(P)   SCCOMM        Q#COM2
     C                   MOVEL(P)  SCDFLR        Q#FOLD
     C                   MOVE      *BLANK        Q#FL01
     C*** BEGIN ADD *** 12/12/02 **************************************
     C     SCDCID        IFEQ      'F0350C'
     C     SCDCID        OREQ      'F0269C1'
     C*** BEGIN ADD *** 03/29/10 **************************************
     C     SCDCID        OREQ      'W0070C'
     C*** END   ADD *** 03/29/10 **************************************
     C                   MOVEL     'Y'           Q#FL01
     C                   ENDIF
     C*** END   ADD *** 12/12/02 **************************************
     C                   ENDIF                                                  DEWR = 'Y'
     C                   MOVEL(P)  SCCDAT        Q#CDAT            8
     C                   MOVEL(P)  QQPNAM        Q#PNAM           10
     C                   MOVE      ##SECL        Q#SECL            3
     C     SCDUSR        IFNE      *BLANKS
     C                   MOVEL(P)  SCDUSR        Q#USER           10
     C                   ELSE                                                   DUSR <> blanks
     C                   MOVEL     Q1USER        Q#USER
     C                   ENDIF                                                  DUSR <> blanks
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C     SCDITP        IFEQ      'Y'
     C     SCDITP        OREQ      'N'
     C                   MOVEL(P)  SCDITP        Q#DIFL            1
     C                   ELSE                                                   DITP = 'Y'
     C                   MOVEL     *BLANK        Q#DIFL
     C                   ENDIF                                                  DITP = 'Y'
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVEL(P)  SCPGFL        Q#FL02            1
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  ##SBYL        Q#SBYL            1
     C                   MOVEL(P)  *BLANKS       Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  *BLANK        Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      '00000'       Q#PIDX            5
     C                   MOVEL(P)  *BLANK        Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     SCCSID        Q#CSID            2
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
     C                   PARM                    Q#CSID
     C                   MOVE      Q#SEQ#        ##SEQ#            5 0
     C*** END ADD *** 04/25/00 ****************************************
     C*
     C*                      IF ADDITIONAL REMARKS ARE REQUESTED
     C*                         (SCARMK='Y') THEN CALL PROGRAM TO ADD
     C*                            REMARKS....
     C     SCARMK        IFEQ      'Y'
     C                   CALL      'SC014R'
     C                   PARM                    QQKEY
     C*** BEGIN CHG *** 04/25/00 **************************************
     C********             PARM           CMSEQ#
     C                   PARM                    ##SEQ#
     C*** END CHG *** 04/25/00 ****************************************
     C                   PARM                    QQPNAM
     C                   ENDIF
     CSR                 ENDSR
     C*====================================================================*
     C* * *   EDTS01  SUBROUTINE - EDIT SCREEN 1 FIELDS    E D T S 0 1   ***
     C*====================================================================*
     CSR   EDTS01        BEGSR
     C*
     C* KEY FIELD CAN NOT BE BLANKS
     C     SCKEY         CABEQ     *BLANKS       STRT                     28
     C*
     C* MUST ENTER VALID DATE
     C*
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCDAT
     C*
     C     SRERR         CABEQ     '1'           STRT                     52
     C*
     C                   MOVE      ' '           #DIARY            1             DIARY TYPE FLAG
     C*
     C*    If remark is secured, the secuity level displayed on screen
     C*      is moved into the field to be stored when record is made
     C*      or updated.
     C*
     C     *IN56         IFEQ      '1'
     C     SCSECL        ANDNE     ##SECL
     C                   MOVEL     SCSECL        ##SECL
     C                   ELSE
     C     *IN56         IFEQ      '0'
     C                   MOVEL     *ZEROS        SCSECL
     C                   MOVEL     SCSECL        ##SECL
     C                   END
     C                   END
     C*
     C*   REMARK TYPE MUST BE ENTERED, AND IT MUST BE VALID
     C                   MOVEL     SCDEPT        SCCODE
     C                   MOVE      SCRMTY        SCCODE
     C*
     C     SCCODE        IFNE      *BLANKS
     C     SCCODE        CHAIN     STCTL1                             51
     C     *IN51         CABEQ     '1'           STRT
     C                   ELSE
     C                   MOVE      '1'           *IN41
     C                   GOTO      STRT
     C                   ENDIF                                                   SCCODE NE *BLANKS
     C*
     C     SCDITP        IFEQ      'Y'
     C*
     C*                       MUST ENTER A VALID USER NAME
     C                   MOVEL     SCDUSR        $$USER
     C     SLKY          CHAIN     PSL01                              58
     C     *IN58         CABEQ     '1'           STRT
     C                   END
     C*
     C     SCDITP        IFNE      ' '
     C     SCDITP        IFNE      'Y'
     C     SCDITP        ANDNE     'N'
     C                   MOVE      '1'           *IN35
     C                   GOTO      STRT
     C                   END                                                    END SCDITP NE Y/N
     C                   ELSE                                                   ELSE SCDITP NE ' '
     C                   MOVE      CTFL01        SCDITP
     C                   END                                                    END SCDITP NE ' '
     C*
     C*  Check Data Entry Flag to see if Data Entry Program has been
     C*     called once already.
     C*
     C     XXDEFL        IFEQ      'N'
     C*
     C*  If DRFL03 = 'Y' in file SDRPF, Remark Code is checked to see
     C*     if it has an associated Data Entry Program ID.
     C*
     C                   MOVE      'N'           XXDEPG            1
     C     SYKY          CHAIN     SDR01                              40
     C     *IN40         IFEQ      '0'
     C     DRFL03        IFEQ      'Y'
     C*** BEGIN CHG *** 03/29/10 **************************************
     C     CTDEPG        IFNE      *BLANKS
     C     CTDEPG        ANDNE     'W0070C'
     C*********  CTDEPG    IFNE *BLANKS
     C*** END   CHG *** 03/29/10 **************************************
     C                   MOVE      'Y'           XXDEPG
     C*
     C     SCDCID        IFNE      *BLANKS
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C*  Check to see if a recursive call was made.  If so... error msg
     C*
     C                   MOVE      *BLANKS       SCDCID
     C                   MOVEL     CTDEPG        SCDCID
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDSDT
     C*
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCDAT
     C*
     C     XXCDAT        IFLE      XXDSDT
     C                   MOVE      *BLANKS       ##PGM
     C                   CALL      'SRCHKPGM'
     C                   PARM      CTDEPG        ##PGM            10
     C                   PARM                    ##ERR             1
     C*
     C     ##ERR         IFEQ      *ON
     C                   WRITE     SCR99
     C                   WRITE     SCR01
     C                   MOVE      '1'           *IN47
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  ALL EDITS PASSED - CALL DATA ENTRY PROGRAM
     C*  (NOTE: It was necessary to break up the comment field of size
     C*   275 into DECOM1 and DECOM2 because the largest alpha-numeric
     C*   value allowed for a parameter is 256.
     C*
     C                   MOVE      *BLANKS       DEKEY
     C                   MOVE      *BLANKS       DECOM1
     C                   MOVE      *BLANKS       DECOM2
     C                   MOVE      *BLANKS       DERTCD
     C                   MOVEL     QQKEY         DEKEY
     C*
     C                   CALL      CTDEPG                               37
     C                   PARM                    DEKEY
     C                   PARM                    DECOM1
     C                   PARM                    DECOM2
     C                   PARM                    DERTCD
     C*
     C     *IN37         IFEQ      '1'
     C                   WRITE     SCR99
     C                   WRITE     SCR01
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   MOVE      'Y'           XXDEFL
     C*
     C     DERTCD        IFEQ      'A'
     C     DERTCD        OREQ      'B'
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     DERTCD        IFEQ      *BLANKS
     C*** BEGIN ADD *** 02/16/07 *************************************
     C     SCCOMM        IFEQ      *BLANKS
     C*** END   ADD *** 02/16/07 *************************************
     C                   MOVE      'Y'           XXDEWR
     C                   MOVE      *BLANKS       SCCOMM
     C     DECOM1        IFEQ      *BLANK
     C     DECOM2        ANDEQ     *BLANK
     C                   MOVEL     CTDESC        SCCOMM
     C                   MOVEL     CTDESC        DECOM1
     C                   ELSE
     C                   MOVEL     DECOM1        SCCOMM
     C                   MOVE      DECOM2        SCCOMM
     C                   END
     C*** BEGIN ADD *** 02/16/07 *************************************
     C                   ENDIF
     C*** END   ADD *** 02/16/07 *************************************
     C*
     C                   MOVE      *BLANKS       SCDCID
XXX  C                   MOVEL     CTDEPG        SCDCID
XXX  C                   MOVEL(P)  QQKEY         SCDFLR
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDSDT
     C*
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCDAT
     C*
     C     XXCDAT        IFLE      XXDSDT
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   END                                                    CTDEPG NE 0
     C                   END                                                    DRFL03 = 'Y'
     C                   END                                                    *IN40 = 0
     C                   END
     C*
     C                   MOVEA     '0000'        *IN(51)
     C                   MOVEA     '0000'        *IN(56)
     C*
     C*** BEGIN ADD *** 03/29/10 **************************************
     C     SCCODE        IFEQ      'CLRWR'
     C*** BEGIN ADD *** 05/31/11 **************************************
     C     SCCODE        OREQ      'CLWCD'
     C*** END   ADD *** 05/31/11 **************************************
     C                   MOVEL     'W0070C'      SCDCID
     C                   ENDIF
     C*** END   ADD *** 03/29/10 **************************************
     C     SCDFLR        IFNE      *BLANKS
     C     SCDCID        ORNE      *BLANKS
     C*** BEGIN ADD *** 12/12/02 **************************************
     C     SCDCID        IFNE      'F0350C'
     C     SCDCID        ANDNE     'F0269C1'
     C*** BEGIN ADD *** 03/29/10 **************************************
     C     SCDCID        ANDNE     'W0070C'
     C*** END   ADD *** 03/29/10 **************************************
     C*** END   ADD *** 12/12/02 **************************************
     C*
     C*** If AS/400 Office NOT Installed - Don't allow this function
     C     XXOFIF        IFEQ      'N'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C     SLADC         IFNE      'Y'
     C                   MOVE      '1'           *IN57                          ADD NOT ALLOWED
     C                   GOTO      STRT
     C                   END
     C*** VALIDATE FOLDER:
     C                   MOVEL     SCDFLR        XXFOLD           80            DEFAULT FOLDER
     C                   MOVE      *BLANK        QQRTCD            1
     C                   CALL      'SVALFLR'
     C                   PARM                    XXFOLD
     C                   PARM                    QQRTCD
     C*** THAT IT EXISTS:
     C     QQRTCD        IFEQ      '1'
     C     SCDCID        ANDNE     'F0269C1'
     C*** BEGIN ADD *** 03/29/10 **************************************
     C     SCDCID        ANDNE     'W0070C'
     C*** END   ADD *** 03/29/10 **************************************
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*** THAT USER HAS AUTHORITY TO IT
     C     QQRTCD        IFEQ      '2'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*** Must have a Default Folder entered for this function
     C     XXFOLD        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C*                     IF ENTERED, DOC ID MUST BE VALID (EXISTING)
     C                   MOVEL     SCDFLR        XXFOLD           80
     C                   MOVEL     SCDCID        XXDCID
     C                   MOVEL     *BLANK        QQRTCD            1
     C                   CALL      'SVALDOC'
     C                   PARM                    XXDCID           12
     C                   PARM                    XXFOLD
     C                   PARM                    QQRTCD
     C*
     C     SCDCID        IFNE      'F0269C1'
     C*** BEGIN ADD *** 03/29/10 **************************************
     C     SCDCID        ANDNE     'W0070C'
     C*** END   ADD *** 03/29/10 **************************************
     C     QQRTCD        CABEQ     '1'           STRT                     54
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 12/12/02 **************************************
     C                   ENDIF
     C*** END   ADD *** 12/12/02 **************************************
     C                   ENDIF                                                  DCID <> *BLANKS
     C*
     C     SCCOMM        CABEQ     *BLANKS       STRT                     53
     C*
     C*** MUST ENTER VALID ADDITIONAL REMARKS FLAG
     C     SCARMK        IFNE      'Y'
     C     SCARMK        CABNE     'N'           STRT                 4646
     C                   END
     C*
     C* PURGEABLE ITEM FLAG MUST BE 'Y', 'N', OR BLANK
     C*
     C     SCPGFL        IFNE      'Y'
     C     SCPGFL        ANDNE     'N'
     C     SCPGFL        ANDNE     *BLANK
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C                   END
     C*
     C* IF PURGEABLE ITEM FLAG IS = 'Y'
     C*
     C     SCPGFL        IFEQ      'Y'
     C*
     C     CTDPRG        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN43
     C                   GOTO      STRT
     C                   END
     C*
     C     SCDITP        IFNE      'Y'
     C                   MOVE      '1'           *IN44
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C* IF PURGEABLE ITEM FLAG IS = *BLANKS
     C*
     C     SCPGFL        IFEQ      *BLANKS
     C     CTDPRG        IFNE      *ZEROS
     C     SCDITP        ANDEQ     'Y'
     C                   MOVE      'Y'           SCPGFL
     C                   ELSE
     C                   MOVE      'N'           SCPGFL
     C                   END
     C                   END
     CSR                 ENDSR
     C*===================================================================
**  CK1 - Command Keys - SCR01
F3=Exit             F5=Add office document               F24=More keys
F1=OV/400       F3=Exit       F5=Add office document     F24=More keys
F6=Display documents in a folder                         F24=More keys
F9=User Search             F10=Table Search              F24=More keys
F8=Scan a Document   F9=User Search   F10=Table Search   F24=More keys
              F20=Usr Exit     F23=Secure Remarks added  F24=More keys
              F20=Usr Exit  F23=Unsecure Remarks added   F24=More keys
