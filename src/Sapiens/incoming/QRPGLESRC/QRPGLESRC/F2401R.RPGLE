     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* REVISIONS:
     H* 02/25/02 C:79 PROJ: FSMNT
     H*               Correct paging issue
     H*
     H* 08/04/16 C:79 Support: 42706/099
     H*               On Add/Change load and protect State
     H*****************************************************************
     H*
     FSTABL     IF   E           K DISK
     FSTSTP     IF   E             DISK
     F                                     RENAME(PST01:PST02)
     FF2401F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFDRUP     IF   E             DISK
     F                                     RENAME(PRU01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFDRUL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PRU01:DBRCDF)
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(10) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(10) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('State Adjudication -
     D                                     Programs')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F2401R02')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(12)
     D** Data structure for XXEFDT
     D                 DS
     D  XXEFDT                 1      8
     D  XXMON                  1      2
     D  XXDAY                  3      4
     D  XXYR                   5      8
     D** Data Structure for INFO1
     D                 DS
     D  INFO1                  1     70
     D  DTST                   2      3
     D  DTEFDT                10     19
     D  DTATYP                28     28
     D  DTAPGM                35     40
     D  DTSTAT                46     46
     D  DTSPAC                47     70

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*** Beg Add *** 08/04/16 ***************************************
     C                   PARM      @@RRN         ##RRN2            9 0
     C*** End Add *** 08/04/16 ***************************************
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
     C     KEY1          KLIST
     C                   KFLD                    KKST
     C                   KFLD                    KKEFDT
     C*** BEG ADD *** 02/25/02 ****************************************
     C                   KFLD                    KKATYP
     C*** END ADD *** 02/25/02 ****************************************
     C     STKY          KLIST
     C                   KFLD                    KKST1            10
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    RUST          KKST
     C     *LIKE         DEFINE    RUEFDT        KKEFDT
     C     *LIKE         DEFINE    RUATYP        KKATYP
     C     *LIKE         DEFINE    RUST          QQST
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C     SSST          IFNE      *BLANKS
     C     SSATYP        ORNE      *BLANKS
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
     C                   DO
     C*
     C** VERIFY STATE IS A VALID
     C*
     C  N90SSST          IFNE      *BLANKS
     C                   MOVEL(P)  SSST          KKST1
     C     STKY          CHAIN     PST01                              99
     C   99              SETON                                        9041
     C                   ENDIF
     C*
     C*
     C*
     C  N90SSATYP        IFNE      'I'
     C     SSATYP        ANDNE     'D'
     C     SSATYP        ANDNE     'X'
     C     SSATYP        ANDNE     'H'
     C     SSATYP        ANDNE     'P'
     C     SSATYP        ANDNE     'A'
     C     SSATYP        ANDNE     'N'
     C     SSATYP        ANDNE     *BLANKS
     C                   SETON                                        9042
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVE(P)   *LOVAL        KKEFDT
     C*** BEG ADD *** 02/25/02 ****************************************
     C                   MOVE      *BLANK        KKATYP
     C*** END ADD *** 02/25/02 ****************************************
     C*** BEG CHG *** 12/14/00 ****************************************
     C******************** MOVE *LOVAL    KKST      P
     C                   MOVE(P)   QQST          KKST
     C*** END CHG *** 12/14/00 ****************************************
     C                   MOVE(P)   *LOVAL        KKATYP
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C     *IN21         IFEQ      *ON
     C   21              MOVEL(P)  SCPST1        KKST
     C                   Z-ADD     0             KKEFDT
     C*** BEG ADD *** 02/25/02 ****************************************
     C                   MOVE      *BLANK        KKATYP
     C*** END ADD *** 02/25/02 ****************************************
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVEL(P)  SCST          KKST
     C                   MOVEL(P)  SCEFDT        KKEFDT
     C*** BEG ADD *** 02/25/02 ****************************************
     C                   MOVE      SCATYP        KKATYP
     C*** END ADD *** 02/25/02 ****************************************
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   CLEAR                   INFO1
     C                   MOVEL(P)  RUST          SCST
     C                   Z-ADD     RUEFDT        SCEFDT
     C*** BEG ADD *** 02/25/02 ****************************************
     C                   MOVE      RUATYP        SCATYP
     C*** END ADD *** 02/25/02 ****************************************
     C                   Z-ADD     RUEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT
     C     XXMON         CAT       '/':0         DTEFDT
     C                   CAT       XXDAY:0       DTEFDT
     C                   CAT       '/':0         DTEFDT
     C                   CAT       XXYR:0        DTEFDT
     C                   MOVE      RUST          DTST
     C*                    MOVE XXEFDT    DTEFDT
     C                   MOVE      RUATYP        DTATYP
     C                   MOVE      RUAPGM        DTAPGM
     C                   MOVE      RUSTAT        DTSTAT
     C                   MOVE      *BLANK        DTSPAC
     C                   MOVE      INFO1         SCDATA
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
     C     @@RRN         CHAIN     PST02                              99
     C                   MOVEL     STKEY         QQST
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21              READ      DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21              READP     DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
     C                   ENDSR
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
     C     SSST          IFNE      *BLANK
     C     SSST          ANDNE     RUST
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSATYP        IFNE      *BLANK
     C     SSATYP        ANDNE     RUATYP
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C*** BEG ADD *** 12/14/00 ****************************************
     C     QQST          IFNE      *BLANK
     C     QQST          ANDNE     RUST
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*** END ADD *** 12/14/00 ****************************************
     C*
     C                   ENDSR
** Column headings
Opt  State  Effective Date  Bill Type  Program  Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YNY
217F17=Subset               YYY
3
