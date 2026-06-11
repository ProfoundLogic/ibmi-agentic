     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE E0035R - MANUAL RATE MAINTENANCE
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* WORK WITH PROGRAM FOR MANUAL RATE MASTER MAINTENANCE          *
     H*                                                               *
     H* CALLED BY: MENU                                               *
     H*                                                               *
     H* CALLS TO: E0035R02                                            *
     H*                                                               *
     H* CREATED: 08/10/01                                             *
     H*                                                               *
     H* 08/10/01 C:136 PROJECT WC577                                  *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/19/24 C:291 Project: WC2952                                *
     H*                Exapand Expected Loss Rate to 8,3.             *
     H*                Changed SCRate field in display file to 8,3    *
     H*                                                               *
     H* 10/18/02 SUPPORT: EX093 34579                                 *
     H*          Expand rate field to 7.0                             *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  60         GENERAL READ/CHAIN                                *
     H*                                                               *
     H*****************************************************************
     FE0035F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FEMMRP     IF   E             DISK
     F                                     RENAME(PMR01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FEMMRL1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PMR01:DBRCDF)
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(11) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(11) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Manual Rate Maintena-
     D                                     nce')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('SYMESG')
     D ACDPGM          C                   CONST('E0035R02')
     D COMAPI          C                   CONST('SA005R')
     D USPAPI          C                   CONST('S0006R')
     D DELETD          C                   CONST('*Deleted')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(12)
     D*================================================================
     D*  Data Structures
     D*================================================================
      *
     D                 DS
     D  VIEW1                  1     70
     D  V1DRSC                 6      8
     D  V1CLAS                19     23
     D  V1EFDT                35     44
      *** BEG CHG **** 10/18/02 ************************************************
     D**********************************     53  58 V1RATE
      *** Beg Chg ***** 06/19/24 ***************************************
     D***  V1RATE                53     60
     D  V1RATE                52     60
      *** End Chg ***** 06/19/24 ***************************************
      *** END CHG **** 10/18/02 ************************************************
     D  V1STAT                66     66
      *
      *** Beg Add ***** 06/19/24 **********************************
     D strRate         S              9
     D intPos          S              3  0
     D intRate         S              8  5
      *** End Add ***** 06/19/24 **********************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    @@DRSC            3
     C                   PARM                    @@CLAS            5 0
     C                   PARM                    @@RDAT            7 0
      *
      *  Add/change/display program parameter list
      *
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##CONM           40
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C*================================================================
     C*  KLIST definitions
     C*================================================================
      *
     C     MRKY1         KLIST
     C                   KFLD                    ##CO#
      *
     C     MRKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKDRSC
     C                   KFLD                    KKCLAS
     C                   KFLD                    KKEFDT
      *
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
      *
     C     *LIKE         DEFINE    MRDRSC        KKDRSC
     C     *LIKE         DEFINE    MRCLAS        KKCLAS
     C     *LIKE         DEFINE    MREFDT        KKEFDT
     C     *LIKE         DEFINE    MREFDT        S1EFDT
      *
     C/COPY SCOPYWW01C
      *
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C     SSDRSC        IFNE      *BLANKS
     C     SSEFDT        ORNE      *ZEROS
     C     SSFRCL        ORNE      *ZEROS
     C     SSTOCL        ORNE      *ZEROS
     C                   MOVE      *ON           *IN11
     C                   ENDIF
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
     C*
     C     *IN90         IFEQ      *OFF
     C     SSEFDT        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SSEFDT
     C*
     C     SRERR         IFEQ      '1'
     C                   SETON                                        71  90
     C*
     C                   ELSE
     C*
     C                   Z-ADD     SSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         S1EFDT
     C*
     C                   ENDIF                                                  IF SRERR = 1
     C                   ENDIF                                                  IF 90 OFF
     C*
     C  N90SSFRCL        IFGT      SSTOCL
     C                   SETON                                        72  90
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C                   MOVE      *BLANKS       SSDRSC
     C                   Z-ADD     0             SSFRCL
     C                   Z-ADD     0             SSTOCL
     C                   Z-ADD     0             SSEFDT
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
      *
     C                   MOVEL     *LOVAL        KKDRSC
     C                   Z-ADD     0             KKCLAS
     C                   Z-ADD     0             KKEFDT
      *
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
      *
      * Following routines prevent KEYSET values from conflicting with
      * SUBSET values. First find last record of file.
      *
     C                   MOVEL     *HIVAL        KKDRSC
     C                   Z-ADD     *HIVAL        KKCLAS
     C                   Z-ADD     *HIVAL        KKEFDT
     C     MRKY2         SETGT     DBRCDF
     C                   READP     DBRCDF                                 60
      *
      * Pick the value of DRSC that doesn't cause the SUBSET to malfunction or
      * that isn't larger than the last record DRSC value.
      *
     C     SSDRSC        IFNE      *BLANKS
     C     SCPST1        ANDNE     SSDRSC
     C                   MOVEL     SSDRSC        KKDRSC
     C                   ELSE
     C                   MOVEL     SCPST1        KKDRSC
     C                   ENDIF
      *
     C     KKDRSC        IFGT      MRDRSC
     C                   MOVEL     MRDRSC        KKDRSC
     C                   ENDIF
      *
      * Reposition file pointer to last record in which MRDRSC equals KKDRSC
      *
     C     MRKY2         SETGT     DBRCDF
     C                   READP     DBRCDF                                 60
      *
      * Pick the CLAS value that doesn't conflict with the SUBSET or is greater
      * than the last record's CLAS value.
      *
     C     SCPSTA        IFNE      *ZEROS
     C     SCPSTA        ANDGT     MRCLAS
     C                   Z-ADD     MRCLAS        KKCLAS
     C                   ELSE
     C                   Z-ADD     SCPSTA        KKCLAS
     C                   ENDIF
      *
     C     SSTOCL        IFNE      *ZEROS
     C     KKCLAS        ANDGT     SSTOCL
     C                   Z-ADD     SSTOCL        KKCLAS
     C                   ENDIF
      *
      * Reposition file pointer to last record in which MRDRSC equals KKDRSC and
      * MRCLAS equals KKCLAS.  Make sure KKCLAS equals a value in the file.
      *
     C     KKCLAS        IFEQ      *ZEROS
     C     MRKY2         SETGT     DBRCDF
     C                   READ      DBRCDF                                 60
     C                   ELSE
     C     MRKY2         SETGT     DBRCDF
     C                   READP     DBRCDF                                 60
     C                   ENDIF
      *
     C     KKCLAS        IFNE      MRCLAS
     C                   READ      DBRCDF                                 60
     C                   Z-ADD     MRCLAS        KKCLAS
     C                   ENDIF
      *
      * Pick the appropriate value for KKEFDT.
      *
     C                   Z-ADD     SCPSTB        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT            7 0
      *
     C     SCPSTB        IFNE      *ZEROS
     C     SSEFDT        ANDNE     *ZEROS
     C     XXEFDT        ANDGT     SSEFDT
     C                   Z-ADD     SSEFDT        KKEFDT
     C                   ELSE
     C                   Z-ADD     XXEFDT        KKEFDT
     C                   ENDIF
      *
     C     KKEFDT        IFGT      MREFDT
     C                   Z-ADD     MREFDT        KKEFDT
     C                   ENDIF
      *
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
      *
     C                   MOVEL     SCDRSC        KKDRSC
      *
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
      *
     C                   Z-ADD     SCCLAS        KKCLAS
      *
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
      *
     C                   SELECT
     C     *IN21         WHENEQ    *ON
      *
     C                   MOVEL     MRDRSC        SCDRSC
     C                   Z-ADD     MRCLAS        SCCLAS
      *
     C                   Z-ADD     MREFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
      *
     C                   Z-ADD     MRRATE        SCRATE
      *
     C                   MOVE      SCDRSC        V1DRSC
      *
     C                   MOVEL(P)  SCCLAS        ##NUM
     C                   MOVEL(P)  '4'           ##MASK
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM
     C                   PARM                    ##MASK
     C                   MOVEL     ##NUM         V1CLAS
      *
     C                   MOVEL(P)  SCEFDT        ##NUM
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM            80
     C                   PARM      'DATE'        ##MASK           10
     C                   MOVEL     ##NUM         V1EFDT
      *
      *** End Chg ***** 06/19/24 ***************************************
     C***                 MOVEL(P)  SCRATE        ##NUM
     C***                 CALL      'SA020R'
     C***                 PARM                    ##NUM
     C***                 PARM      '4'           ##MASK
     C***                 PARM      4             ##DEC             3 0
      **** BEG CHG **** 10/18/02 ************************************************
     C********************* MOVEL##NUM     XXRATE  6
     C***                 MOVEL     ##NUM         XXRATE            8
      **** END CHG **** 10/18/02 ************************************************
     C***   ' ':'0'       XLATE     XXRATE        V1RATE
       Clear V1Rate;
       If MRRate <> *Zeros;
          intRate = MRRate;
          intRate = intRate/100;
          strRate = %Char(intRate);
          intPos =  %Scan(' ':strRate);
          If intPos <> 0;
             V1Rate = %Replace(%Subst(strRate:1:intPos-1):V1Rate:
              (%Len(strRate)-(intPos-1))+1);
          Else;
             V1Rate = strRate;
          Endif;
       Endif;
      *** End Chg ***** 06/19/24 ***************************************
     C                   MOVE      MRSTAT        V1STAT
      *
     C                   MOVEL     VIEW1         SCDATA
      *
     C                   ENDSL
      *
     C                   ENDSR
     C*================================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20 CHANGE STATUS
     C*================================================================
     C     OPT20         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C     SCSEL         CASEQ     '20'          OPT20
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   MOVE      *ON           *INLR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21MRKY1         READE     DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21MRKY1         READPE    DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
      *
     C*          SSDRSC    IFNE *BLANKS
     C*                    MOVELSSDRSC    KKDRSC    P
     C*                    ENDIF
      *
     C*          SSTOCL    IFNE *ZEROS
     C*                    MOVELSSDRSC    KKDRSC    P
     C*                    Z-ADDSSTOCL    KKCLAS
     C*                    ENDIF
      *
     C*          SSEFDT    IFNE *ZEROS
     C*                    MOVELSSDRSC    KKDRSC    P
     C*                    Z-ADDSSTOCL    KKCLAS
     C*                    Z-ADDSSEFDT    SRDTI
     C*                    CALL 'SRCVTDAT'DTCYMD
     C*                    MOVELSRDTO     KKEFDT
     C*                    ENDIF
      *
     C   21MRKY2         SETGT     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
      *
     C*          SSDRSC    IFNE *BLANKS
     C*                    MOVELSSDRSC    KKDRSC    P
     C*                    ENDIF
      *
     C*          SSFRCL    IFNE *ZEROS
     C*                    MOVELSSDRSC    KKDRSC    P
     C*                    Z-ADDSSFRCL    KKCLAS
     C*                    ENDIF
      *
     C*          SSEFDT    IFNE *ZEROS
     C*                    MOVELSSDRSC    KKDRSC    P
     C*                    Z-ADDSSTOCL    KKCLAS
     C*                    Z-ADDSSEFDT    SRDTI
     C*                    CALL 'SRCVTDAT'DTCYMD
     C*                    MOVELSRDTO     KKEFDT
     C*                    ENDIF
      *
     C   21MRKY2         SETLL     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C                   MOVEL(P)  DELETD        SCDATA
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C                   MOVE      SCDRSC        @@DRSC
     C                   Z-ADD     SCCLAS        @@CLAS
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         @@RDAT
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
      *
     C     @@CALL        IFEQ      '2'
     C     @@DRSC        ANDNE     *BLANKS
     C     @@DRSC        ANDNE     MRDRSC
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C     @@CALL        IFEQ      '2'
     C     @@CLAS        ANDNE     *ZEROS
     C     @@CLAS        ANDNE     MRCLAS
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C     SSDRSC        IFNE      *BLANKS
     C     SSDRSC        ANDNE     MRDRSC
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSDRSC
      *
     C     SSEFDT        IFNE      *ZEROS
     C     S1EFDT        ANDNE     MREFDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSEFDT
      *
     C     SSFRCL        IFNE      *ZEROS
     C     SSFRCL        ANDGT     MRCLAS
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSFRCL
      *
     C     SSTOCL        IFNE      *ZEROS
     C     SSTOCL        ANDLT     MRCLAS
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  TOFRCL
      *
     C                   ENDSR
** Column headings
Opt  Rate Set Code   Payroll Class   Effective Date   Manual Rate   Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               NNY
217F17=Subset               YYY
3
