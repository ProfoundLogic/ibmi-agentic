     H Copyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H BNDDIR('STBNDDIR')
     H/define CONVERT
     H*
     H****************************************************************
     H*                                                              *
     H*  CREATED:  07/14/98   C:94  PROJECT:  MC179                  *
     H*                                                              *
     H****************************************************************
     H*  REVISIONS:                                                  *
     H*                                                              *
     H*  05/20/24 C:342 Project MC446                                *
     H*                 Managed Care CO Maintenance - UX Updates     *
     H*                                                              *
     H*  02/16/10 C:62 PROJECT 25WQK                                 *
     H*                ADD CONTACT HEADING OPTION                    *
     H*                                                              *
     H****************************************************************
     FS0050F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FSMCOP     IF   E             DISK
     F                                     RENAME(PCO01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FSMCOL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCO01:DBRCDF)
     FSMCOL1    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PCO01:DBRCD2)
      /COPY SCOPYWW01D
     d****** Beg Add *** 05/20/24 *******************************************
      /Copy SprGenPop
     d****** End Add *** 05/20/24 *******************************************
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(15) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(15) ALT(OF)
     D*
     D DATA            S             70    DIM(2)
     D DTA1            S              1    DIM(70)                              VIEW 1 DATA
     D DTA2            S              1    DIM(70)                              VIEW 2 DATA
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Companies')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('S0050R01')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D SF1PAG          C                   CONST(10)
     D CHGNUM          C                   CONST('SA020R')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0
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
     C*  CHANGE NUMBER TO ELIMINATE LEADING ZEROS
     C*
     C     CHGPL         PLIST
     C                   PARM                    ##NUM            80
     C                   PARM      *BLANKS       ##MASK           10
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C     KEY2          KLIST
     C                   KFLD                    KKNAME
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    COCO#         KKCO#
     C     *LIKE         DEFINE    CONAME        KKNAME
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
     C                   Z-ADD     *ZEROS        KKCO#
     C                   MOVEL     *BLANKS       KKNAME
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C   21              MOVE      SCPST1        KKCO#
     C   22              MOVEL     SCPST2        KKNAME
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   Z-ADD     SCCO#         KKCO#
     C                   MOVEL(P)  SCNAME        KKNAME
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     COCO#         SCCO#
     C                   MOVEL     CONAME        SCNAME
     C*
     C                   MOVEL(P)  COCO#         ##NUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     ##NUM         XXCO#             3
     C                   MOVE      COSTAT        SCSTAT            1
     C*
     C                   CLEAR                   SCDATA
     C*
     C                   SELECT
     C*
     C*** VIEW 1 ***
     C*
     C     HD#           WHENEQ    1
     C                   Z-ADD     1             Y                 2 0
     C                   MOVEA     XXCO#         DTA1(Y)
     C                   ADD       6             Y
     C                   MOVEA     SCNAME        DTA1(Y)
     C                   ADD       53            Y
     C                   MOVEA     SCSTAT        DTA1(Y)
     C                   MOVEA     DTA1          SCDATA
     C*
     C*** VIEW 2 ***
     C*
     C     HD#           WHENEQ    2
     C                   Z-ADD     1             Y                 2 0
     C                   MOVEA     SCNAME        DTA2(Y)
     C                   ADD       43            Y
     C                   MOVEA     XXCO#         DTA2(Y)
     C                   ADD       16            Y
     C                   MOVEA     SCSTAT        DTA2(Y)
     C                   MOVEA     DTA2          SCDATA
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* OPT09 - SUBROUTINE FOR OPTION 09
     C*================================================================
     C*
     C* OPTION 09=WCMC SETUP INFO
     C*
     C     OPT09         BEGSR
     C*
     C     @@CALL        IFEQ      ' '
     C*
     C                   MOVE      'C'           ##MODE
     C*
     C                   ELSE
     C*
     C                   MOVEL     'D'           ##MODE
     C*
     C                   ENDIF
     C*
     C                   CALL      'F0052R01'
     C                   PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C                   ENDSR
     C*================================================================
     C* OPT10 - SUBROUTINE FOR OPTION 10
     C*================================================================
     C*
     C* OPTION 10=WCMC STATE INFO
     C*
     C     OPT10         BEGSR
     C*
     C                   CALL      'F0053R'
     C                   PARM      @@CALL        ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C                   ENDSR
     C*================================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*================================================================
     C*
     C* OPTION 20=CHANGE STATUS
     C*
     C     OPT20         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL                  99
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 02/16/10 **************************************
     C*================================================================
     C* OPT25 - CONTACT HEADINGS
     C*================================================================
     C     OPT25         BEGSR
     C*
     C                   CALL      'S0225R'
     C                   PARM      @@CALL        ##CALL
     C     *INKC         PARM      *BLANKS       ##KC
     C     *INKL         PARM      *BLANKS       ##KL
     C                   PARM      SCCO#         ##CO#
     C*
     C                   ENDSR
     C*** END   ADD *** 02/16/10 **************************************
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C     SCSEL         CASEQ     '9'           OPT09
     C     SCSEL         CASEQ     '10'          OPT10
     C     SCSEL         CASEQ     '20'          OPT20
     C*** BEGIN ADD *** 02/16/10 **************************************
     C     SCSEL         CASEQ     '25'          OPT25
     C*** END   ADD *** 02/16/10 **************************************
     C                   ENDCS
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
     C***  Begin Add  ***  05/20/24  **************************************
      /free
        PopOneBtn('This Menu Option is no Longer in Use. Please Use the ' +
         'Company Configuration Menu Option': 'Use Company Configuration':'Ok');
          *InLr = *On;
          Return;
      /end-free
     C***  End   Add  ***  05/20/24  **************************************
     C***  Begin Add  ***  02/16/10  **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C                   SELECT
     C     CLTID         WHENEQ    '025'
     C                   MOVEL     '1'           XXOPFN
     C                   MOVEL     '25'          XXNMBR
     C                   MOVEL     'YYN'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDSL
     C***  End   Add  ***  02/16/10  **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21              READ      DBRCDF                                 95
     C   22              READ      DBRCD2                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21              READP     DBRCDF                                 95
     C   22              READP     DBRCD2                                 95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C   22KEY2          SETGT     DBRCD2                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
     C   22KEY2          SETLL     DBRCD2                             96
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
     C*
     C                   Z-ADD     SCDBRR        @@RRN
     C*
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C                   ENDSR
** Column headings
Opt Co#   Company name                                       Status
Opt Company name                               Co#           Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
1099=WCMC setup info        YYY
11010=WCMC state info       YYY
12020=Change Status         YNN
12525=Contact Headings      NNN
203F3=Exit                  YYN
205F5=Refresh               YNN
206F6=Add                   YNN
211F11=Change View          YYY
212F12=Cancel               NNY
217F17=Subset               NNN
3
