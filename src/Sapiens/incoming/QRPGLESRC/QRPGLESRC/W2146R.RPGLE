     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE W2146R - WORK WITH PAYMENT VALUE 1 CODES
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                                 *
     H* 11/15/12 C:287 Project: MC367                                   *
     H*                Disabled option 5 and function code F17 for GS   *
     H*                                                               *
     H* 03/12/09 C:220 SUPPORT: 67514
     H*                         Subset not working correctly on PV1
     H*                         When selects the A/I records.
     H*
     H* 08/28/06 C:180 PROJECT: 114WMF
     H*                         Add entry parameter to accept group.
     H*                         Add subset entry for Minor Paymt code.
     H*                         Applicable to spefic client.
     H*
     H*
     H* 02/15/01 C:15  Project: WC577                                 *
     H*                Added a subset for the reserve code.  If called*
     H*                from the request pmt screen, subset the search *
     H*                to display only PV1 codes with the appropriate *
     H*                reserve code.                                  *
     H*****************************************************************
     FW2146F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWTB2P     IF   E             DISK
     F                                     RENAME(PB201:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWTABL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PB201:DBRCDF)
      /COPY SCOPYWW01D
     D*
     D HD              S             74    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(11) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(11) ALT(OF)
     D*** BEGIN ADD *** 02/15/01 **************************************
     D*** END ADD ***** 02/15/01 **************************************
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*** BEGIN ADD *** 02/15/01 **************************************
     D                 DS                  INZ
     D  RC                     1     20
     D                                     DIM(10)                              Reserve codes
     D  B2RC01                 1      2
     D  B2RC02                 3      4
     D  B2RC03                 5      6
     D  B2RC04                 7      8
     D  B2RC05                 9     10
     D  B2RC06                11     12
     D  B2RC07                13     14
     D  B2RC08                15     16
     D  B2RC09                17     18
     D  B2RC10                19     20
     D*** END ADD ***** 02/15/01 **************************************
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Payment Value 1 -
     D                                     Codes')
     D SLMSGI          C                   CONST('WC39998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W2146R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@KEY
     C*** BEGIN ADD *** 02/15/01 **************************************
     C                   PARM                    @@RECD            2
     C*** END ADD ***** 02/15/01 **************************************
     C*
     C*++ BEG ADD +++ 08/28/06 ++++++++++++++++++++++++++++++++++++++++
     C                   PARM                    @@FND             3
     C*++ END ADD +++ 08/28/06 ++++++++++++++++++++++++++++++++++++++++
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
     C     *LIKE         DEFINE    B2KEY         KKKEY
     C     *LIKE         DEFINE    B2KEY         @@KEY
     C*** BEGIN ADD *** 02/15/01 **************************************
     C                   Z-ADD     0             X                 3 0
     C*** END ADD ***** 02/15/01 **************************************
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
B01  C     SSSTAT        IFNE      *BLANKS
     C                   MOVE      *ON           *IN11
E01  C                   ENDIF
     C*
     C*** BEGIN ADD *** 02/15/01 **************************************
     C     SSRECD        IFNE      *BLANKS
     C                   MOVE      *ON           *IN11
     C                   ENDIF
     C*** END ADD ***** 02/15/01 **************************************
     C*
     C*++ BEG ADD +++ 08/28/06 ++++++++++++++++++++++++++++++++++++++++
     C     SSMINR        IFNE      *BLANKS
     C                   MOVE      *ON           *IN11
     C                   ENDIF
     C*++ END ADD +++ 08/28/06 ++++++++++++++++++++++++++++++++++++++++
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
     C*** BEGIN ADD *** 02/15/01 **************************************
     C*  Process searches
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C*  Reserve code search
     C*
     C     HFLD          IFEQ      'SSRECD'
     C*
     C                   CALL      'W2136R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    QQKEY            10
     C*
     C     QQKEY         IFEQ      *BLANKS
     C     *INKL         OREQ      *ON
     C                   MOVE      *ON           *IN87
     C                   ELSE
     C                   MOVEL     QQKEY         SSRECD
     C                   ENDIF                                                  QQKEY = *BLANKS
     C*
     C                   ENDIF                                                  HFLD SSRECD
     C*
     C                   MOVE      *ON           *IN90
     C*++ BEG ADD +++ 08/28/06 ++++++++++++++++++++++++++++++++++++++++
     C     HFLD          IFEQ      'SSMINR'
     C                   SETON                                          8990
     C                   ENDIF                                                  HFLD SSRECD
     C*++ END ADD +++ 08/28/06 ++++++++++++++++++++++++++++++++++++++++
     C                   ENDIF                                                  *INKJ
     C*
     C     SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        8990
     C                   ENDIF
     C*
     C*** Begin add *** 02/16/06 **************************************
     C     SSRECD        IFNE      *BLANKS
     C*** End add ***** 02/16/06 **************************************
     C*** BEGIN ADD *** 02/15/01 **************************************
     C                   MOVEL(P)  SSRECD        KKKEY
     C     KEY2          CHAIN     PRT01                              99
     C   99              SETON                                          8890
     C*** END ADD ***** 02/15/01 **************************************
     C*** Begin add *** 02/16/06 **************************************
     C                   ENDIF
     C*** End add ***** 02/16/06 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C     @@CALL        IFEQ      '2'
     C                   MOVEL     'A'           SSSTAT
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 02/15/01 **************************************
     C     Q1PRMS        IFGT      4
     C                   MOVE      @@RECD        SSRECD
     C                   ENDIF
     C*** END ADD ***** 02/15/01 **************************************
     C*
     C*++ BEG ADD +++ 08/28/06 ++++++++++++++++++++++++++++++++++++++
     C     CLTID         IFEQ      '114'
     C                   MOVE      *ON           *IN50
     C     Q1PRMS        IFGT      5
     C     @@FND         IFEQ      '440'
     C                   MOVE      '440'         SSMINR
     C                   MOVE      *ON           *IN40
     C                   ELSE
     C                   MOVE      *BLANKS       SSMINR
     C                   MOVE      *OFF          *IN40
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C                   MOVE      *OFF          *IN50
     C                   ENDIF
     C*++ END ADD +++ 08/28/06 ++++++++++++++++++++++++++++++++++++++
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
     C                   MOVEL     B2STAT        SCSTAT
     C                   MOVEL     B2KEY         SCKEY
     C                   MOVEL     B2PLCD        SCPLCD
     C                   MOVEL     B2RMCD        SCRMCD
     C                   MOVE      B2NDDT        SCNDDT
     C*
     C                   MOVEL(P)  B2KEY         SCDATA
     C                   MOVE      B2DESC        SCDATA
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
      *++ BEG ADD +++++ 08/28/06 ++++++++++++++++++++++++++++++++++++++
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      *++ END ADD +++++ 08/28/06 ++++++++++++++++++++++++++++++++++++++
      *
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
     C     SSSTAT        ANDNE     B2STAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*** BEGIN ADD *** 03/12/09 **************************************
     C     MEETSS        IFEQ      *OFF
     C                   GOTO      SSTAG
     C                   ENDIF
     C*** END   ADD *** 03/12/09 **************************************
     C*
     C*** BEGIN ADD *** 02/15/01 **************************************
     C     SSRECD        IFNE      *BLANKS
     C*
     C                   MOVE      *OFF          MEETSS
     C                   DO        10            X
     C*
     C     SSRECD        IFEQ      RC(X)
     C                   MOVE      *ON           MEETSS
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 03/12/09 **************************************
     C     SSTAG         TAG
     C*** END   ADD *** 03/12/09 **************************************
     C*** END ADD ***** 02/15/01 **************************************
     **
     C*++ BEG ADD +++ 08/28/06 ++++++++++++++++++++++++++++++++++++++++
     C*
     C     SSMINR        IFNE      *BLANKS
     C*
     C                   MOVE      SSMINR        S2MINR            3 0
     C                   MOVE      S2MINR        S3MINR            3
     C     S3MINR        IFNE      B2MINR
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
     C*
     C*++ END ADD +++ 08/28/06 ++++++++++++++++++++++++++++++++++++++++
     C*
     C                   ENDSR
** Column headings
Code Description                     Placement  Days    DARTS    Sts
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
