     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 11/15/12 C:287 Project: MC367                                 *
     H*                Disabled option 5 and function code F17 for GS *
     H*                                                               *
     H* 02/26/07   C:185  PROJECT:114WMF, Back to base.               *
     H*                   114WBY Dated 06/11/01.                      *
     H*                                                               *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FW2151F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWTB7P     IF   E             DISK
     F                                     RENAME(PB701:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWTABL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PB701:DBRCDF)
      /COPY SCOPYWW01D
     D*
     D HD              S             74    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(11) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(11) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*** Begin add *** 02/26/07 **************************************
     D                 DS
     D  HDR2                   1     74
     D  CW                    58     70
     D*** Begin END *** 02/26/07 **************************************
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Payment Value 2 -
     D                                     Codes')
     D SLMSGI          C                   CONST('WC39998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W2151R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D*** Begin add *** 02/26/07 **************************************
     D CWHEAD          C                   CONST('CW usage only')

     I*** Begin END *** 02/26/07 **************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@KEY
     C*** Begin add *** 02/26/07 **************************************
     C                   PARM                    @@FND             3
     C*** End add ***** 02/26/07 **************************************
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
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C*
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    B7KEY         KKKEY
     C     *LIKE         DEFINE    B7KEY         @@KEY
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C*** Begin add *** 02/26/07 **************************************
     C     CLTID         IFNE      '114'
     C*** End   add *** 02/26/07 **************************************
B01  C     SSSTAT        IFNE      *BLANKS
     C                   MOVE      *ON           *IN11
E01  C                   ENDIF
     C*** Begin add *** 02/26/07 **************************************
     C                   ELSE
     C     SSSTAT        IFNE      *BLANKS
     C     SSRSTR        ORNE      *BLANKS
     C                   MOVE      *ON           *IN11
     C                   ENDIF
     C                   ENDIF
     C*** End   add *** 02/26/07 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* CVTDT - SUBROUTINE TO CONVERT DATE FROM CYMD TO MDYY
     C*================================================================
     C     CVTDT         BEGSR
     C*
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
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
     C*
     C     SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        8990
     C                   ENDIF
     C*
     C*** Begin add *** 02/26/07 **************************************
     C     CLTID         IFEQ      '114'
     C     SSRSTR        IFNE      ' '
     C     SSRSTR        ANDNE     'Y'
     C     SSRSTR        ANDNE     'N'
     C                   SETON                                        8890
     C                   ENDIF
     C                   ENDIF
     C*** End add ***** 02/26/07 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C     @@CALL        IFEQ      '2'
     C*
     C                   MOVEL     'A'           SSSTAT
     C*
     C                   ENDIF
     C*
     C*** Begin add *** 02/26/07 **************************************
     C     CLTID         IFEQ      '114'
     C     Q1PRMS        IFGT      4
     C     @@FND         IFEQ      '440'
     C                   MOVEL     'Y'           SSRSTR
     C                   ELSE
     C                   MOVEL     'N'           SSRSTR
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     ' '           SSRSTR
     C                   ENDIF
     C                   ENDIF
     C*** End add ***** 02/26/07 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVE      *LOVAL        KKKEY
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   MOVEL     SCPST1        KKKEY
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVEL     SCKEY         KKKEY
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     B7STAT        SCSTAT
     C                   MOVEL     B7KEY         SCKEY
     C                   MOVEL     B7LTLF        SCLTLF
     C*
     C*** Begin add *** 02/26/07 **************************************
     C     CLTID         IFEQ      '114'
     C                   MOVEL     B7RSTR        SCRSTR
     C                   ELSE
     C                   MOVEL     *BLANKS       SCRSTR
     C                   ENDIF
     C*** End add ***** 02/26/07 **************************************
     C*
     C                   MOVEL(P)  B7KEY         SCDATA
     C                   MOVE      B7DESC        SCDATA
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
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKC         CASEQ     *ON           KCDO
     C     *INKX         CASEQ     *ON           KXDO
     C                   CAS                     ENTRDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '20'          OPT20
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   MOVE      *ON           *IN20
     C                   Z-ADD     11            SF1PAG            5 0
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C*
     C***  Begin Add  ***  02/26/07  **********************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C     CLTID         IFEQ      '114'
     C                   MOVE      *ON           *IN16
     C                   MOVEA     HD(1)         HDR2
     C                   MOVE      CWHEAD        CW
     C                   MOVEA     HDR2          HD(1)
     C                   ELSE
     C                   MOVE      *OFF          *IN16
     C                   ENDIF
     C***  End   Add  ***  02/26/07  **********************************
     C*
     C*** Beg Add ** MC367 *** 11/15/12 ******************************
      /Free
      /If Not Defined(WEB)
       //Disable option 05
           XXOpfn = '1';
           XXMIS = 'NNN';
           XXNmbr = '05';
           Exsr UPDOFA;
       //Disable Function 17
           XXOpfn = '2';
           XXMIS = 'NNN';
           XXNmbr = '17';
           Exsr UPDOFA;
      /EndIf
      /End-Free
     C*** End Add ** MC367 *** 11/15/12 ******************************
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21KEY1          READE     DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY1          READPE    DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY2          SETGT     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY2          SETLL     DBRCDF                             96
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
     C                   MOVEL     SCKEY         @@KEY
     C*
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     B7STAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C*** Begin add *** 02/26/07 **************************************
     C     CLTID         IFEQ      '114'
     C     SSRSTR        IFNE      *BLANKS
     C     SSRSTR        ANDNE     B7RSTR
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
     C*** End add ***** 02/26/07 **************************************
     C*
     C                   ENDSR
** Column headings
Code Description                         Sts  Lost time
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               NNY
213F13=User parms           YYN
217F17=Subset               YYY
3
