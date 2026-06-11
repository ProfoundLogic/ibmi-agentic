     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*                                                               *
     H*****************************************************************
     H*  F8194R01 - Upload Bill Information                           *
     H*                                                               *
     H*  Called from: F8194R - Work with 837 Bills with Errors        *
     H*                                                               *
     H*  Calls to   : F8193R - 837 Bill Upload                        *
     H*                                                               *
     H*****************************************************************
     H*  Created on 11/30/2017 by c:79 for Project 114MJB             *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H*  06/09/20  C:212 Project : SY420                              *
     H*                  Mask SSN                                     *
     H*                                                               *
     H*  04/05/19 C:079 Support: 114/78417                            *
     H*                 Add Ins ID and Suffix                         *
     H*****************************************************************
     H*  Indicators used:                                             *
     H*                                                               *
     H*****************************************************************
     FF8194F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFACLMP    UF A E             DISK
     F                                     RENAME(PFACLM:DBRCDF)
     FFAHDRL    IF   E           K DISK
     FFMC1L     UF   E           K DISK
     FFMCMLB    IF   E           K DISK
     FFMCML1    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FFMCMP     IF   E             DISK
     F                                     RENAME(PCM01:PCM03)
     FFMPDL     IF   E           K DISK
     FNMNWL     IF   E           K DISK

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************

      /COPY SCOPYMT01D
     D*
     D CLM             S              1    DIM(20)
     D*
     D*
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D                 DS
     D  VALUE                  1     11
     D  SIGN                   1      1
     D  AMOUNT                 2     11  2
     D                 DS
     D  CLAIM#                 1     20
     D  GROUP                  1      3
     D  YEAR                   4      6
     D  CASE                   7     13
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('837 Bill -
     D                                     Transfer')
     D OPTNMT          C                   CONST('837 Bill -
     D                                     Transfer')
     D OPTNMD          C                   CONST('837 Bill -
     D                                     Deletion')
     D COMAPI          C                   CONST('FA007R')
     D XXZVAL          C                   CONST('0000000000000')
     d xxDt8           s              8  0
     d dtCCYYMMDD      s              8  0

     I*** End Add *** 12/14/10 ****************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RTCD            1
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#
     C                   PARM                    ##FYR
     C*
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*YYMD'       SRFFMT            8
     C                   PARM      '*MDYY'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYM2        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*YYMD'       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     C1KY          KLIST
     C                   KFLD                    ##CO#
     C*
     C     HDKY1         KLIST
     C                   KFLD                    KKICNTL
     C                   KFLD                    KKGCTL
     C                   KFLD                    KKTSHCNTL
     C                   KFLD                    KKBHTOATI
     C*
     C     CMKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSS#
     C                   KFLD                    KKACDT
     C*
     C     CMKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##SUB#
     C                   KFLD                    ##FND
     C                   KFLD                    ##FYR
     C                   KFLD                    ##CASE
     C*
     C     CMKY3         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C*
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    CMSS#         KKSS#
     C     *LIKE         DEFINE    CMACDT        KKACDT
     C     *LIKE         DEFINE    CMSS#         ##SS#
     C     *LIKE         DEFINE    CMCO#         ##CO#
     C     *LIKE         DEFINE    CMSUB#        ##SUB#
     C     *LIKE         DEFINE    CMFND         ##FND
     C     *LIKE         DEFINE    CMFYR         ##FYR
     C     *LIKE         DEFINE    CMCASE        ##CASE
     C     *LIKE         DEFINE    CMSUB#        KKSUB#
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C     *LIKE         DEFINE    PDFEIN        KKFEIN
     C     *LIKE         DEFINE    PDMOD         KKMOD
     C     *LIKE         DEFINE    PDFEIN        HDFEIN
     C     *LIKE         DEFINE    PDMOD         HDMOD
     C     *LIKE         DEFINE    CMFND         KKWCF#
     C     *LIKE         DEFINE    CMDT          XXFRDT
     C     *LIKE         DEFINE    CMDT          XXTODT
     C     *LIKE         DEFINE    HDICNTL       KKICNTL
     C     *LIKE         DEFINE    HDTSHGCTL     KKGCTL
     C     *LIKE         DEFINE    HDTSHCNTL     KKTSHCNTL
     C     *LIKE         DEFINE    HDBHTOATI     KKBHTOATI
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK - Error checking for why this bill wasn't processed.
     C*================================================================
     C     ERRCK         BEGSR
     C*
     C                   SETOFF                                       212223
     C                   SETOFF                                       2425
     C*
     C* LETS SEE IF THIS IS A RECONSIDERATION
     C*
     C                   EXSR      GETCLM
     C*
     C     CLMPRFL       IFEQ      'E'
     C*
     C* Check WCMC Claim Master to see if SS#/ACDT exists.
     C*
     C     CMKY3         CHAIN     PCM02                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   Z-ADD     *ZEROS        COUNT             3 0
     C     CLMPIRIQ      IFEQ      'SY'
     C                   MOVEL(P)  CLMPIPCCN     KKSS#
     C                   ELSE
     C                   MOVE      *ZEROS        KKSS#
     C                   ENDIF
     C*
     C                   MOVEL     CLMAD         xxDT8
     C                   Z-ADD     xxDT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKACDT
     C*
     C                   ELSE
     C                   Z-ADD     1             COUNT
     C*
     C                   ENDIF
     C*
     C                   SELECT
     C*
     C* SS#/ACDT doesn't exist.
     C*
     C     COUNT         WHENEQ    0
     C                   SETON                                        21
     C*
     C* Let's get the WCMS group number
     C*
     C     COUNT         WHENEQ    1
     C                   MOVE      ##CO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C                   MOVE      CMSUB#        SCSUB#
     C                   MOVE      CMFND         SCFND
     C                   MOVE      CMFYR         SCFYR
     C                   MOVE      CMCASE        SCCASE
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM      *BLANKS       #1WCF#            3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C*
     C     ##RTCD        IFEQ      *BLANKS
     C                   MOVE      #1WCF#        KKWCF#
     C                   ELSE
     C                   Z-ADD     *ZEROS        KKWCF#
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C* FEIN/MOD does not exist.
     C*
     C                   MOVEL     HDBPTAXID     KKFEIN
     C                   Z-ADD     *ZEROS        KKMOD
     C*
     C     PDKY          CHAIN     PPD01                              99
      /Free
       If *in99 = *on;                                           //Provider not found.
          *in22 = *on;
          xxChg = 'Y';
       Elseif PDstat <> 'A' and PDstat <> *blanks;             //Check if    prov. is active.
             *in22 = *on;
             xxChg = 'Y';
       Endif;
      /End-Free
     C*
     C                   ELSE
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C     *IN16         IFEQ      *OFF
     C*
     C* Check WCMC Claim Master to see if SS#/ACDT exists.
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   Z-ADD     SCSUB#        KKSUB#
     C                   Z-ADD     SCFND         KKFND
     C                   Z-ADD     SCFYR         KKFYR
     C                   Z-ADD     SCCASE        KKCASE
     C     CMKY3         CHAIN     PCM02                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   Z-ADD     *ZEROS        COUNT             3 0
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVEL     SCSS#         KKSS#
      /Free
       kkSS# = UnMaskData('SSN':'SCSS#':scSS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKACDT
     C*
     C     CMKY1         CHAIN     PCM01                              99
     C     *IN99         DOWEQ     *OFF
     C                   ADD       1             COUNT
     C     CMKY1         READE     PCM01                                  99
     C                   ENDDO
     C                   ELSE
     C                   Z-ADD     1             COUNT
     C                   ENDIF
     C*
     C                   SELECT
     C*
     C* SS#/ACDT doesn't exist.
     C*
     C     COUNT         WHENEQ    0
     C                   SETON                                        7190
     C*
     C* SS#/ACDT occurs more than once.
     C*
     C     COUNT         WHENGT    1
     C                   SETON                                        75
     C*
     C                   ENDSL
     C*
     C                   ENDIF
     C*
     C* FEIN/MOD does not exist.
     C*
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCFEIN        KKFEIN
     C                   Z-ADD     SCMOD         KKMOD
     C*
     C     PDKY          CHAIN     PPD01                              99
      /Free
       If *in99 = *on;                                           //Provider not found.
          *in72 = *on;
          *in90 = *on;
       Elseif PDstat <> 'A' and PDstat <> *blanks;             //Check if    prov. is active.
          *in72 = *on;
          *in90 = *on;
       Endif;
      /End-Free
     C                   ENDIF
     C*
     C* HAS THE PROVIDER CHANGED
     C*
     C     *IN90         IFEQ      *OFF
     C     XXCHG         ANDNE     'Y'
     C     SCFEIN        IFNE      HDFEIN
     C     SCMOD         ORNE      HDMOD
     C                   MOVEL     'Y'           XXCHG
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
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
     C                   ENDSR
     C*================================================================
     C* GETCLM - GET THE CLAIM NUMBER
     C*================================================================
     C     GETCLM        BEGSR
     C*
     C                   MOVEL     *BLANKS       XXCLM#           20
     C                   Z-ADD     *ZEROS        XXONE             1 0
     C                   Z-ADD     *ZEROS        XXTWO             2 0
     C                   Z-ADD     *ZEROS        XXTHR             3 0
     C                   Z-ADD     *ZEROS        XXFOU             4 0
     C                   Z-ADD     *ZEROS        XXFIV             5 0
     C                   Z-ADD     *ZEROS        XXSIX             6 0
     C                   Z-ADD     *ZEROS        XXSEV             7 0
     C                   Z-ADD     *ZEROS        KKWCF#
     C                   Z-ADD     *ZEROS        KKSUB#
     C                   Z-ADD     *ZEROS        KKFND
     C                   Z-ADD     *ZEROS        KKFYR
     C                   Z-ADD     *ZEROS        KKCASE
     C*
     C* LET'S EXTRACT THE GROUP
     C*
     C                   CLEAR                   CLM
     C     CLMPATPCQU    IFEQ      'Y4'
     C                   MOVEL     CLMPATCLM#    ZZCLM#           20
     C                   ELSE
     C     CLMPIRIQ      IFEQ      '1W'
     C                   MOVEL     CLMPIPCCN     ZZCLM#
     C                   ELSE
     C                   MOVEL     CLMSUBSPI     ZZCLM#
     C                   ENDIF
     C                   ENDIF
     C                   MOVEL     ZZCLM#        CLM
     C                   Z-ADD     1             X                 3 0
     C     '-'           LOOKUP    CLM(X)                                 99
     C     *IN99         IFEQ      *ON
     C                   SELECT
     C     X             WHENEQ    2
     C                   MOVEL     ZZCLM#        XXONE
     C                   Z-ADD     XXONE         KKWCF#
     C     X             WHENEQ    3
     C                   MOVEL     ZZCLM#        XXTWO
     C                   Z-ADD     XXTWO         KKWCF#
     C     X             WHENEQ    4
     C                   MOVEL     ZZCLM#        XXTHR
     C                   Z-ADD     XXTHR         KKWCF#
     C                   ENDSL
     C*
     C                   ADD       1             X
     C                   MOVEA(P)  CLM(X)        ZZCLM#
     C*
     C* LET'S GET THE YEAR
     C*
     C                   CLEAR                   CLM
     C                   MOVEA     XXCLM#        CLM
     C                   Z-ADD     1             X                 3 0
     C     '-'           LOOKUP    CLM(X)                                 99
     C     *IN99         IFEQ      *ON
     C                   SELECT
     C     X             WHENEQ    2
     C                   MOVEL     XXCLM#        XXONE
     C                   Z-ADD     XXONE         SCFYR
     C     X             WHENEQ    3
     C                   MOVEL     XXCLM#        XXTWO
     C                   Z-ADD     XXTWO         SCFYR
     C     X             WHENEQ    4
     C                   MOVEL     XXCLM#        XXTHR
     C                   Z-ADD     XXTHR         SCFYR
     C                   ENDSL
     C                   ADD       1             X
     C                   MOVEA(P)  CLM(X)        XXCLM#
     C                   ENDIF
     C*
     C* LET'S GET THE YEAR
     C*
     C                   CLEAR                   CLM
     C                   MOVEA     ZZCLM#        CLM
     C                   Z-ADD     1             X                 3 0
     C     ' '           LOOKUP    CLM(X)                                 99
     C     *IN99         IFEQ      *ON
     C                   SELECT
     C     X             WHENEQ    2
     C                   MOVEL     ZZCLM#        XXONE
     C                   Z-ADD     XXONE         SCCASE
     C     X             WHENEQ    3
     C                   MOVEL     ZZCLM#        XXTWO
     C                   Z-ADD     XXTWO         SCCASE
     C     X             WHENEQ    4
     C                   MOVEL     ZZCLM#        XXTHR
     C                   Z-ADD     XXTHR         SCCASE
     C     X             WHENEQ    5
     C                   MOVEL     ZZCLM#        XXFOU
     C                   Z-ADD     XXFOU         SCCASE
     C     X             WHENEQ    6
     C                   MOVEL     ZZCLM#        XXFIV
     C                   Z-ADD     XXFIV         SCCASE
     C     X             WHENEQ    7
     C                   MOVEL     ZZCLM#        XXSIX
     C                   Z-ADD     XXSIX         SCCASE
     C     X             WHENEQ    8
     C                   MOVEL     ZZCLM#        XXSEV
     C                   Z-ADD     XXSEV         SCCASE
     C                   ENDSL
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     ZZCLM#        CLAIM#
      /Free
                         Monitor;
                         kkWcF# = %Dec(Group:3:0);
                         On-Error;
                         kkWcF# = 0;
                         Endmon;

                         Monitor;
                         scFyr  = %Dec(Year:3:0);
                         On-Error;
                         scFyr  = 0;
                         Endmon;

                         Monitor;
                         scCase = %Dec(Case:7:0);
                         On-Error;
                         scCase = 0;
                         Endmon;
      /End-free
     C                   ENDIF
     C*
     C                   MOVE      ##CO#         #1CO#
     C                   MOVE      KKWCF#        #1WCF#
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1WCF#            3
     C                   PARM      *BLANKS       #1SUB#            5
     C                   PARM      *BLANKS       #1FND             3
     C*
     C     ##RTCD        IFEQ      *BLANKS
     C                   MOVE      #1SUB#        SCSUB#
     C                   MOVE      #1FND         SCFND
     C                   ELSE
     C                   Z-ADD     *ZEROS        SCSUB#
     C                   Z-ADD     *ZEROS        SCFND
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=Search
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCSUB#'
     C     HFLD          OREQ      'SCFND'
     C     HFLD          OREQ      'SCFYR'
     C     HFLD          OREQ      'SCCASE'
     C*
     C                   CALL      'F0269R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##SUB#
     C                   PARM      *ZEROS        ##FND
     C                   PARM      *ZEROS        ##FYR
     C                   PARM      *ZEROS        ##CASE
     C                   PARM      *BLANKS       ##SS#
     C                   PARM      *ZEROS        ##ACDT            8 0
     C                   PARM      ' '           ##RTCD            1
     C*
     C     ##RTCD        IFEQ      'A'
     C     ##RTCD        OREQ      'B'
     C     ##FYR         OREQ      *ZEROS
     C     ##CASE        OREQ      *ZEROS
     C                   SETON                                        3190
     C                   ELSE
     C     CMKY2         CHAIN     PCM02                              99
     C*
     C                   MOVEL(P)  CMLSN         ##LAST
     C                   MOVEL(P)  CMFSN         ##FRST
     C                   Z-ADD     CMSUB#        SCSUB#
     C                   Z-ADD     CMFND         SCFND
     C                   Z-ADD     CMFYR         SCFYR
     C                   Z-ADD     CMCASE        SCCASE
     C                   MOVE      '1'           ##TERM
     C*
     C                   CALL      'SRNAME'
     C                   PARM                    ##LAST           56
     C                   PARM                    ##FRST           15
     C                   PARM      '1'           ##TERM            1
     C*
     C                   MOVEL(P)  ##LAST        SCCNAM
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVEL(P)  ##SS#         SCSS#
      /Free
       scSS# = MaskData('SSN':'##SS#':##SS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C                   Z-ADD     ##ACDT        SCACDT
     C                   SETON                                        32
     C*
     C                   ENDIF
     C*
     C* PROVIDER SEARCH
     C*
     C     HFLD          WHENEQ    'SCFEIN'
     C     HFLD          OREQ      'SCMOD'
     C*
     C                   CALL      'F0032R'
     C                   PARM      '2'           ##CALL
     C                   PARM      *BLANKS       ##FEIN            9
     C                   PARM      *BLANKS       ##MOD             3
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM      *BLANKS       ##TYPE            1
     C*
     C     ##RTCD        IFEQ      'A'
     C     ##RTCD        OREQ      'B'
     C     ##FEIN        OREQ      *BLANKS
     C                   SETON                                        9032
     C                   ELSE
     C                   MOVE      ##FEIN        SCFEIN
     C                   MOVE      ##MOD         SCMOD
     C*
     C                   CALL      'FA725R'
     C                   PARM      SCFEIN        #1FEIN            9 0
     C                   PARM      SCMOD         #1MOD             3 0
     C                   PARM      *BLANKS       #1NAME           40
     C                   PARM      *BLANKS       #1ERR             1
     C     #1ERR         IFNE      '0'
     C                   MOVEL(P)  #1NAME        SCPNAM
     C                   ENDIF
     C                   ENDIF                                                  ##RTCD = A
     C*
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C* Get Header Record
     C*
     C                   EVAL      KKICNTL = CLMICNTL
     C                   EVAL      KKGCTL = CLMGCTL
     C                   EVAL      KKTSHCNTL = CLMTSCNTL
     C                   EVAL      KKBHTOATI = CLMBHTTID
     C     HDKY1         CHAIN     PFAHDR                             99
     C*
     C     *IN99         IFEQ      *OFF
     C                   ENDIF
     C*
     C                   MOVEL(P)  CLMPACCT      SCTRID
     C*
     C     CLMsubIrc     IFNE      '18'
     C                   MOVEL(P)  CLMPATLSN     XXLSN            56
     C                   MOVEL(P)  CLMPATFSN     XXFSN            15
     C*
     C                   ELSE
     C                   MOVEL(P)  CLMSUBLSN     XXLSN            56
     C                   MOVEL(P)  CLMSUBFSN     XXFSN            15
     C*
     C                   ENDIF
     C*
     C                   CALL      'SRNAME'
     C                   PARM      XXLSN         ##LAST           56
     C                   PARM      XXFSN         ##FRST           15
     C                   PARM      '1'           ##TERM            1
     C*
     C                   MOVEL(P)  ##LAST        SCCNAM
     C*
     C     CLMPIRIQ      IFEQ      'SY'
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVEL(P)  CLMPIPCCN     SCSS#
      /Free
       scSS# = MaskData('SSN':'CLMPIPCCN':CLMPIPCCN);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C                   ENDIF
     C*
     C                   MOVEL     CLMAD         xxDT8
     C                   Z-ADD     xxDT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         SCACDT
     C*
     C                   MOVEL     CLMPATDOB     xxDT8
     C                   Z-ADD     xxDT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         SCDOB
     C*
     C                   Z-ADD     *ZEROS        SCMOD
     C*
     C                   MOVEL     HDBPTAXID     KKFEIN
     C                   MOVEL     HDBPTAXID     SCFEIN
     C                   MOVE      *ZEROS        KKMOD
     C*
     C     PDKY          CHAIN     PPD01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     PDFSN         IFEQ      *BLANKS
     C                   MOVEL(P)  PDNAME        SCPNAM
     C                   ELSE
     C     PDFSN         CAT(P)    PDNAME:1      SCPNAM
     C                   ENDIF
     C*
     C                   MOVEL(P)  PDADR1        SCPAD1
     C                   MOVEL(P)  PDCTY         SCPCTY
     C                   MOVEL(P)  PDST          SCPST
     C                   MOVE      PDZIP         SCPZIP
     C*
     C                   ENDIF
     C*
     C     *IN16         IFEQ      *OFF
     C                   EXSR      ERRCK
     C                   ENDIF
     C*
     C                   SETOFF                                       17
     C                   SETOFF                                       2627
     C                   MOVE      *BLANKS       XXDERR            1
     C     XXDERR        IFNE      *BLANKS
     C     XXDERR        IFEQ      'D'
     C                   SETON                                        26
     C                   ELSE
     C                   SETON                                        27
     C                   ENDIF
     C                   SETON                                        17
     C                   ENDIF
     C     CLMPRFL       IFEQ      'R'
     C                   SETON                                        17
     C                   ENDIF
     C     XXDERR        IFEQ      'R'
     C                   SETON                                        29
     C                   ENDIF
     C*
      /Free
                         scChrg = %Dec(CLMch:9:2);
       //*** Beg Add *** 04/05/19 *****************************************
                         scInsID = CLMinsID;
                         scPaSfx = CLMpaSfx;
       //*** End Add *** 04/05/19 *****************************************
      /End-Free
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C     *IN16         IFEQ      *ON
     C*
     C                   MOVEL(P)  'D'           CLMPRFL
     C*
     C                   Z-ADD     *ZEROS        CLMCO#
     C                   Z-ADD     *ZEROS        CLMSUB#
     C                   Z-ADD     *ZEROS        CLMFND
     C                   Z-ADD     *ZEROS        CLMFYR
     C                   Z-ADD     *ZEROS        CLMCASE
     C                   Z-ADD     *ZEROS        CLMEOB#
     C                   Z-ADD     *ZEROS        CLMMOD
     C                   Z-ADD     *ZEROS        CLMACDT
     C*
     C                   ELSE
     C*
     C                   Z-ADD     CMCO#         CLMCO#
     C                   Z-ADD     CMSUB#        CLMSUB#
     C                   Z-ADD     CMFND         CLMFND
     C                   Z-ADD     CMFYR         CLMFYR
     C                   Z-ADD     CMCASE        CLMCASE
     C                   Z-ADD     CMACDT        CLMACDT
     C                   MOVEL     CMSS#         CLMSS#
     C                   Z-ADD     PDFEIN        CLMFEIN
     C                   Z-ADD     PDMOD         CLMMOD
     C*** Beg Add *** 04/05/19 *********************************************
      /Free
                         CLMinsID = scInsID;
                         CLMpaSfx = scPaSfx;
      /End-free
     C*** End Add *** 04/05/19 *********************************************
     C*
     C     *IN91         IFEQ      *OFF
     C     CLMEOB#       IFEQ      *ZEROS
     C     C1KY          CHAIN     PC101                              99
     C     *IN99         IFEQ      *OFF
     C                   ADD       1             C1EOB#
     C                   UPDATE    PC101
     C                   ENDIF
     C                   Z-ADD     C1EOB#        CLMEOB#
     C                   ENDIF
     C                   ELSE
     C                   ENDIF
     C*
     C                   Z-ADD     *ZEROS        CLMRCN
     C*
     C                   ENDIF
     C*
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
     C                   SETOFF                                       3132
     C*
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   SETOFF                                       0916
     C                   SETOFF                                       212223
     C                   SETOFF                                       2425
     C                   SETOFF                                       91
     C*
     C     @@MODE        COMP      'D'                                    09
     C     @@CALL        COMP      'D'                                    16
     C*
     C                   Z-ADD     1             #OFSCR            1 0
     C                   MOVEL     'N'           XXCHG             1
     C*
     C     *IN16         IFEQ      *OFF
     C                   MOVEL(P)  OPTNMT        ##DATA
     C                   ELSE
     C                   MOVEL(P)  OPTNMD        ##DATA
     C                   ENDIF
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDX
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C                   PARM                    ##RHAB            1            REHAB INST
     C                   PARM                    ##MSS             1            MS SUPPORT
     C                   PARM                    ##FSCH            1            FEE SCHEDULE
     C                   PARM                    ##PRVB            1            PROVIDER BILL
     C                   PARM                    ##NPWC            1            NON-PDS WC  LS
     C*
     C     ##WCIF        IFEQ      'Y'
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
     C*
     C     *IN16         IFEQ      *OFF
     C     *IN91         ANDEQ     *OFF
     C                   CALL      'F8193R'
     C                   PARM      'T'           ##MODE            1            Document ID
     C                   PARM      HDICNTL       ##BCHCNTL         9            FAU9P RRN
     C                   PARM      @@RRN         ##RRN             9 0          FAU1P RRN
     C                   PARM      *BLANKS       ##DCID            1            PROV CHANGE
     C                   PARM      *BLANKS       ##RTCD            1            PROV CHANGE
     C                   ENDIF
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      FACLMP
     C*
     C                   ENDSR
