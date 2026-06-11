     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F0354R - WORK WITH PREVIOUS LINES
     H*****************************************************************
     H**   WORK WITH PREVIOUS LINES                                  **
     H**                                                             **
     H**   PROGRAMMER: C:56                                          **
     H**   CREATED:   10/23/98      PROJECT: MC152                   **
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  04/04/00 C:117 PROJECT MCMNT
     H*                 Changed function F15 to "Display Lines" rather
     H*                 than "Previous Lines".
     H*
     H*  09/23/99 C:56  MC212
     H*             1.  Load KKDSEQ so that code description can be
     H*                 accessed.
     H*             2.  Add logic to get Dental description.
     H*****************************************************************
     H**
     FF0354F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFDCAP     IF   E             DISK
     F                                     RENAME(PCA01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFDCAL10   IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCA01:DBRCDF)
     FFAEOP     IF   E             DISK
     FFMPRL     IF   E           K DISK
     FFMSVL1    IF   E           K DISK
     FFDDPL     IF   E           K DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(10) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(10) ALT(OF)
     D*
     D DTA1            S              1    DIM(75)
     D***PY SCOPYWW01E
     D*
     D*
     D                 DS
     D  KKCMSV                 1     11
     D  KKRVCD                 1      3
     D  KKHCPC                 4      8
     D  KKUNTS                 9     11
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Lines')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F0354R02')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(7)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN1            9 0          FAEOP
     C*** BEG ADD *** 04/04/00 ****************************************
     C                   PARM                    @@LIN#            9 0
     C*** END ADD *** 04/04/00 ****************************************
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
     C*
     C     CHGPL         PLIST
     C                   PARM                    QQNUM            80
     C                   PARM                    QQMASK           10
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKLIN#
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKEOB#
     C*
     C* KEY LIST FOR EOMB PHYS/HCFA BILL DETAIL TRANSACTION FILE
     C*
     C     DHKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     PRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCPT4
     C*
     C     PRKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCPT4
     C                   KFLD                    KKEFDT
     C*
     C     SVKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKCMSV
     C                   KFLD                    KKEFDT
     C*
     C     SVKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKCMSV
     C*
     C     DPKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CACO#         KKCO#
     C     *LIKE         DEFINE    CASUB#        KKSUB#
     C     *LIKE         DEFINE    CAFND         KKFND
     C     *LIKE         DEFINE    CAFYR         KKFYR
     C     *LIKE         DEFINE    CACASE        KKCASE
     C     *LIKE         DEFINE    CAFEIN        KKFEIN
     C     *LIKE         DEFINE    CAMOD         KKMOD
     C     *LIKE         DEFINE    CAEOB#        KKEOB#
     C     *LIKE         DEFINE    CALIN#        KKLIN#
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *LIKE         DEFINE    CACPT4        KKCPT4
     C     *LIKE         DEFINE    PREFDT        KKEFDT
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
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C* GET EOMB RECORD AND SET KEY FIELDS
     C*
     C     @@RRN1        CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     EOCO#         KKCO#
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOFEIN        KKFEIN
     C                   Z-ADD     EOMOD         KKMOD
     C                   Z-ADD     EOEOB#        KKEOB#
     C                   ENDIF                                                  *IN99 = *OFF
     C                   Z-ADD     0             KKLIN#
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C   21              MOVE      SCPST1        KKLIN#
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   Z-ADD     SCSUB#        KKSUB#
     C                   Z-ADD     SCFND         KKFND
     C                   Z-ADD     SCFYR         KKFYR
     C                   Z-ADD     SCCASE        KKCASE
     C                   Z-ADD     SCFEIN        KKFEIN
     C                   Z-ADD     SCMOD         KKMOD
     C                   Z-ADD     SCEOB#        KKEOB#
     C                   Z-ADD     SCLIN#        KKLIN#
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   Z-ADD     CALIN#        SCLIN#
     C                   MOVEL(P)  CALIN#        ##NUM
     C                   MOVE(P)   *BLANKS       ##MASK
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM            80
     C                   PARM                    ##MASK           10
     C                   MOVEL     ##NUM         XXNUM             9
     C                   MOVE      XXNUM         XXLIN#            4
     C*** BEG ADD *** 04/04/00 ****************************************
     C                   MOVEL(P)  @@LIN#        ##NUM
     C                   MOVE(P)   *BLANKS       ##MASK
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM            80
     C                   PARM                    ##MASK           10
     C                   MOVEL     ##NUM         XXNUM             9
     C                   MOVE      XXNUM         HLLIN#            4
     C*
     C     HLLIN#        IFEQ      XXLIN#
     C                   SETON                                        61
     C                   ELSE
     C                   SETOFF                                       61
     C                   ENDIF
     C*** END ADD *** 04/04/00 ****************************************
     C*
     C                   Z-ADD     CAFDAT        SRDTI                          from service
     C                   CALL      'SRCVTDAT'    DTMDYY                            date
     C                   MOVEL     SRDTO         HLDATE            8
     C                   MOVEL(P)  HLDATE        QQNUM
     C                   MOVEL(P)  'DATE'        QQMASK
     C                   CALL      'SA020R'      CHGPL
     C                   MOVEL     QQNUM         SCFDAT
     C*
     C*** BEG ADD *** 09/23/99 ****************************************
     C                   Z-ADD     CADSEQ        KKDSEQ
     C*** END ADD *** 09/23/99 ****************************************
     C*
     C*          Get description based on type of bill
     C*          ------------------------------------------------------
     C                   SELECT
     C     EOBTYP        WHENEQ    'N'
     C     EOBTYP        OREQ      'V'
     C*** BEG ADD *** 09/23/99 ****************************************
     C     EOBTYP        OREQ      'D'
     C*** END ADD *** 09/23/99 ****************************************
     C                   MOVEL(P)  CACPT4        XXCPT4           12
     C                   MOVEL(P)  CACPT4        KKCPT4
     C                   Z-ADD     CAFDAT        KKEFDT
     C     PRKY2         SETGT     PPR01
     C     PRKY          READPE    PPR01                                  99
     C  N99              MOVEL(P)  PRDSC1        X1DESC           25
     C   99              MOVE      *BLANKS       X1DESC
     C*
     C     EOBTYP        WHENEQ    'H'
     C                   MOVEL     CACPT4        KKHCPC
     C                   MOVE      CASVCD        KKRVCD
     C                   MOVE      *ZEROS        KKUNTS
     C                   Z-ADD     CAFDAT        KKEFDT
     C                   MOVEL(P)  KKCMSV        XXCPT4
     C     SVKY1         SETGT     PSV01
     C     SVKY2         READPE    PSV01                                  99
     C  N99              MOVEL(P)  SVDESC        X1DESC
     C   99              MOVE      *BLANKS       X1DESC
     C*
     C     EOBTYP        WHENEQ    'X'
     C     DPKY          CHAIN     PDP01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DPNDC         XXCPT4
     C                   MOVEL(P)  DPDESC        X1DESC
     C                   ELSE
     C                   MOVE      *BLANKS       X1DESC
     C                   ENDIF                                                  *IN99 = *Off
     C                   ENDSL
     C*
     C*          Format charge
     C*          ------------------------------------------------------
     C                   MOVEL(P)  CACHRG        QQNUM
     C                   MOVEL(P)  'X,XXX.XX'    QQMASK
     C                   CALL      'SA020R'
     C                   PARM                    QQNUM
     C                   PARM                    QQMASK
     C                   MOVEL     QQNUM         SCCHRG
     C                   MOVE      SCCHRG        ALCHRG           12
     C*
     C*          Set key for paging
     C*          ------------------------------------------------------
     C                   Z-ADD     CASUB#        SCSUB#
     C                   Z-ADD     CAFND         SCFND
     C                   Z-ADD     CAFYR         SCFYR
     C                   Z-ADD     CACASE        SCCASE
     C                   Z-ADD     CAFEIN        SCFEIN
     C                   Z-ADD     CAMOD         SCMOD
     C                   Z-ADD     CAEOB#        SCEOB#
     C                   Z-ADD     CALIN#        SCLIN#
     C*
     C*          Create subfile line
     C*          ------------------------------------------------------
     C                   MOVEL(P)  CACPT4        SCCODE
     C                   Z-ADD     1             Y                 2 0
     C                   MOVEA     XXLIN#        DTA1(Y)
     C                   Z-ADD     6             Y
     C                   MOVEA     SCFDAT        DTA1(Y)
     C                   Z-ADD     17            Y
     C                   MOVEA     SCCODE        DTA1(Y)
     C                   Z-ADD     23            Y
     C                   MOVEA     X1DESC        DTA1(Y)
     C                   Z-ADD     49            Y
     C                   MOVEA     ALCHRG        DTA1(Y)
     C*
     C                   MOVEA     DTA1          SCDAT1
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
     C   21KEY2          READE     DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY2          READPE    DBRCDF                                 95
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
     C                   ENDSR
** Column headings
Opt
** Description--------------MISN
1011=Select                 NNN
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                NNN
203F3=Exit                  NNN
205F5=Refresh               NNN
206F6=Add                   NNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
