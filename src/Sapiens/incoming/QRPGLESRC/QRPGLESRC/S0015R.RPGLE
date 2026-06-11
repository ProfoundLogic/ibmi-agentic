     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE S0015R - W/W OPTION/FUNCTION SPECIAL AUTHORITY
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  01/12/11  C:235 PROJECT: WC1795                              *
     H*                  MAKE SECURITY LEVEL DIFF IN QUOTE/LIVE       *
     H*                                                               *
     H*  11/01/00  C:33  PROJECT: SY043                               *
     H*                  CHANGE PROGRAM TO RECOGNIZE NEW KEY STRUCTURE*
     H*****************************************************************
     FS0015F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FSDOFP     IF   E             DISK
     F                                     RENAME(POF01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FSDOFL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(POF01:DBRCDF)
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D***  BEGIN CHG  ***  11/01/00  *********************************
     D**********          OF      1   8  1   OFD    35   Options, Function
     D OF              S              1    DIM(9) CTDATA PERRCD(1)              Options, Function
     D OFD             S             35    DIM(9) ALT(OF)
     D***  END   CHG  ***  11/01/00  *********************************
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Option/Function -
     D                                     Authority')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('SYMESG')
     D ACDPGM          C                   CONST('S0015R1')
     D COMAPI          C                   CONST('SA005R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(12)
     D*================================================================
     D* Program specific constants
     D*================================================================
     D DELETD          C                   CONST('*Deleted')

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
     C                   PARM                    ##CONM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKPGID
     C                   KFLD                    KKTYPE
     C                   KFLD                    KKNBR
     C***  BEGIN ADD  ***  11/01/00  *********************************
     C                   KFLD                    KKSEQ#
     C***  END   ADD  ***  11/01/00  *********************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    OFPGID        KKPGID
     C     *LIKE         DEFINE    OFTYPE        KKTYPE
     C     *LIKE         DEFINE    OFNBR         KKNBR
     C***  BEGIN ADD  ***  11/01/00  *********************************
     C     *LIKE         DEFINE    OFSEQ#        KKSEQ#
     C***  END   ADD  ***  11/01/00  *********************************
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C***  BEGIN ADD  ***  11/01/00  *********************************
     C     SSPGID        IFNE      *BLANK
     C                   MOVE      *ON           *IN11
     C                   ENDIF                                                  SSPGID<>*BLANKS
     C***  END   ADD  ***  11/01/00  *********************************
     C*
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
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C***  BEGIN ADD  ***  11/01/00  *********************************
     C                   MOVEL     *BLANKS       SSPGID
     C***  END   ADD  ***  11/01/00  *********************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVEL     *LOVAL        KKPGID
     C                   MOVEL     *LOVAL        KKTYPE
     C                   MOVEL     *LOVAL        KKNBR
     C***  BEGIN ADD  ***  11/01/00  *********************************
     C                   MOVEL     *LOVAL        KKSEQ#
     C***  END   ADD  ***  11/01/00  *********************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   MOVEL     SCPST1        KKPGID
     C                   MOVEL     *LOVAL        KKTYPE
     C                   MOVEL     *LOVAL        KKNBR
     C***  BEGIN ADD  ***  11/01/00  *********************************
     C                   MOVEL     *LOVAL        KKSEQ#
     C***  END   ADD  ***  11/01/00  *********************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVEL     SCPGID        KKPGID
     C                   MOVEL     SCTYPE        KKTYPE
     C                   MOVEL     SCNBR         KKNBR
     C***  BEGIN ADD  ***  11/01/00  *********************************
     C                   MOVEL     SCSEQ#        KKSEQ#
     C***  END   ADD  ***  11/01/00  *********************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   MOVEL     OFPGID        SCPGID
     C                   MOVEL     OFTYPE        SCTYPE
     C***  BEGIN ADD  ***  11/01/00  *********************************
     C                   MOVEL     OFSEQ#        SCSEQ#
     C***  END   ADD  ***  11/01/00  *********************************
     C***  BEG   ADD  ***  01/12/11  *********************************
     C                   MOVEL     OFQTLV        SCQTLV
     C***  END   ADD  ***  01/12/11  *********************************
     C*
     C                   SELECT
     C     OFTYPE        WHENEQ    '1'
     C                   MOVEL(P)  'Option'      SCTYPD
     C     OFTYPE        WHENEQ    '2'
     C                   MOVEL(P)  'Function'    SCTYPD
     C                   ENDSL
     C                   MOVEL     OFNBR         SCNBR
     C                   MOVEL     OFTY01        SCTY01
     C                   MOVEL     OFTY02        SCTY02
     C                   MOVEL     OFTY03        SCTY03
     C                   MOVEL     OFTY04        SCTY04
     C                   MOVEL     OFTY05        SCTY05
     C                   MOVEL     OFTY06        SCTY06
     C                   MOVEL     OFLVL         SCLVL
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
     C                   MOVEL(P)  DELETD        SCPGID
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
     C***  BEGIN ADD  ***  11/01/00  *********************************
     C                   SELECT
     C     SSPGID        WHENNE    *BLANKS
     C     SSPGID        ANDNE     OFPGID
     C                   MOVE      *OFF          MEETSS
     C                   ENDSL
     C***  END   ADD  ***  11/01/00  *********************************
     C*
     C                   ENDSR
** Column headings
Opt  Program ID   Type    Nbr   Security types     Level  Env
** Description--------------MISN
1022=Change                 YNN
1033=Copy                   YNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
217F17=Subset               YYY
3
