     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE W8070R7 - EDI/NOI DATA ELEMENT ARRAY EDITOR
     H*
     H*****************************************************************
     H*                                                               *
     H*  CREATED: 06/18/97  for Version 3 BASE
     H*                    C:58  PROJECT: WC468
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 04/10/17 C:180  Support: 46658                                *
     H*                 Fix EDI error 63.                             *
     H*                                                               *
     H* 07/02/13 C:190  Project: 147WBF((Additional changes)          *
     H*                 Currently, field XXACLC is being populated    *
     H*                 from DN,15. With this project DN,15 can be    *
     H*                 populated with the alternate claim number     *
     H*                 which we can no longer use as a key to chain  *
     H*                 WMCMP. Add the fund, fyr & case to incomming  *
     H*                 parameters.
     H*                                                               *
     H* 08/11/10 C:220  Project: 139WNN / PRISS# 54                   *
     H*                 Correct WA076R API call position.             *
     H*                                                               *
     H* 03/24/10 C:232  Project: 139WNN                               *
     H*                 Update programs to use the new EDI Control API*
     H*                 WA076R instead of using WMIXP.                *
     H*                                                               *
     H* 02/04/08 C:53  Project: WC1584                                *
     H*                ALLOW ADDITIONAL MTC A49'S TO BE SENT W/FL     *
     H*                                                               *
     H* 09/14/07 C:53  Support: 60119                                 *
     H*                Correct SIC/NAIC editing                       *
     H*                                                               *
     H* 06/14/07 C:53  Support: 54390                                 *
     H*                Check for inactive records                     *
     H*                                                               *
     H* 01/27/06 C:53  Support: 49459                                 *
     H*                Correct processing.                            *
     H*                                                               *
     H* 10/24/05 C:53  Support: 49459                                 *
     H*                Add error 112 processing.                      *
     H*                                                               *
     H* 03/08/04 C:15  Project: WC468                                 *
     H*                Added PA specific edit.                        *
     H*                                                               *
     H* 05/06/03 C:15  Support: WC072/37121                           *
     H*                Fixed error # 63.                              *
     H*                                                               *
     H* 03/30/03 C:15  Support: WC072/35391                           *
     H*                Updated error # 63.  Added code to allow multi *
     H*                A49's, but to prevent them from being sent out *
     H*                of sequence.                                   *
     H*                                                               *
     H* 07/10/02 C:53  Project: WC468                                 *
     H*                Edit #days & wage period if AWW entered        *
     H*
     H* 04/27/00 C:53  SUPPORT: WC065/19768
     H*                SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*                                                               *
     H* 08/27/98 C:15  Project: 72WBS                                 *
     H*                Add edit to require an A49 'IP' to be accepted *
     H*                before an A49 'FN' or 'S1' can be sent.        *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* 11 - Chain indicator on log file.                             *
     H* 12 - Chain indicator on all other data base files.            *
     H* LR - Indicates when program is to be ended with all files     *
     H*      closed.                                                  *
     H*****************************************************************
     H*
     FWTEML     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PNC01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PRT01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PB201)
     F                                     IGNORE(PB701)
     F                                     IGNORE(PBS01)
     F                                     IGNORE(PTT01)
     F                                     IGNORE(PCC01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PCN01)
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PLR01)
     F                                     IGNORE(PVS01)
     F                                     IGNORE(PTR01)
     F                                     IGNORE(PHY01)
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PBA01)
     F                                     IGNORE(PID01)
     F                                     IGNORE(PIS01)
     FWTGTL     IF   E           K DISK
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
     F*** BEGIN ADD *** 08/27/98 *************************************
     FWMLGL4    IF   E           K DISK    USROPN
     F*** BEGIN DLT *** 03/24/10 **************************************
     F***WMIXL   IF  E           K        DISK
     F*** END DLT ***** 03/24/10 **************************************
     F*** END ADD ***** 08/27/98 *************************************
     F*** Begin add *** 03/30/03 **************************************
     FWMCML1    IF   E           K DISK
     F*** End add ***** 03/30/03 **************************************
     F*

     D*===============================================================
     D DN              S            150    DIM(999)                             Data Elements
     D*
     D*===============================================================
     D                 DS
     D  XXTIM4                 1      4  0
     D  XXMM                   3      4  0
     D*** BEGIN ADD *** 08/27/98 *************************************
     D                 DS
     D  XXACLC                 1     25
     D  XXFND                  4      6  0
     D  XXFYR                  7      9  0
     D  XXCASE                10     16  0
     D*** END ADD ***** 08/27/98 *************************************
     D*** BEG ADD *** 01/27/06 *************************************
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     I*** END ADD *** 01/27/06 *************************************
     I*

     C*================================================================
     C*
     C*--------  MAINLINE -----------------------
     C*
     C*   1. For the specified data element number, retrieve the
     C*      entry in the data element criteria table.
     C*   2. For the specified data element number, retrieve the
     C*      entry(s) in the edit matrix table.
     C*   3. For each entry in the edit matrix table, execute the
     C*      appropriate edit routine.
     C*   4. Pass back the string containing the error numbers, if
     C*      any, for the element number.
     C*   5. When the program is called with a zero element number,
     C*      terminate the program with LR on.
     C*
     C                   EXSR      SRINIT
     C*
     C     EMKY          CHAIN     PEM01                              11
     C*
     C     *IN11         DOWEQ     *OFF
     C*
     C*** BEG ADD *** 06/14/07 ****************************
     C     EMSTAT        IFNE      'I'
     C*** END ADD *** 06/14/07 ****************************
     C*
     C     EMSET         IFEQ      DN(1)
     C     EMSET         OREQ      *BLANKS
     C*
     C     EMMTC         IFEQ      DN(2)
     C     EMMTC         OREQ      *BLANKS
     C*
     C                   SELECT
     C*
     C     EMMSG#        WHENEQ    1
     C                   EXSR      SRE001
     C*
     C     EMMSG#        WHENEQ    2
     C                   EXSR      SRE002
     C*
     C     EMMSG#        WHENEQ    3
     C                   EXSR      SRE003
     C*
     C     EMMSG#        WHENEQ    5
     C                   EXSR      SRE005
     C*
     C     EMMSG#        WHENEQ    6
     C                   EXSR      SRE006
     C*
     C     EMMSG#        WHENEQ    7
     C                   EXSR      SRE007
     C*
     C     EMMSG#        WHENEQ    8
     C                   EXSR      SRE008
     C*
     C     EMMSG#        WHENEQ    9
     C                   EXSR      SRE009
     C*
     C     EMMSG#        WHENEQ    10
     C                   EXSR      SRE010
     C*
     C     EMMSG#        WHENEQ    11
     C                   EXSR      SRE011
     C*
     C     EMMSG#        WHENEQ    12
     C                   EXSR      SRE012
     C*
     C     EMMSG#        WHENEQ    13
     C                   EXSR      SRE013
     C*
     C     EMMSG#        WHENEQ    14
     C                   EXSR      SRE014
     C*
     C     EMMSG#        WHENEQ    15
     C                   EXSR      SRE015
     C*
     C     EMMSG#        WHENEQ    16
     C                   EXSR      SRE016
     C*
     C     EMMSG#        WHENEQ    17
     C                   EXSR      SRE017
     C*
     C     EMMSG#        WHENEQ    18
     C                   EXSR      SRE018
     C*
     C     EMMSG#        WHENEQ    19
     C                   EXSR      SRE019
     C*
     C     EMMSG#        WHENEQ    20
     C                   EXSR      SRE020
     C*
     C     EMMSG#        WHENEQ    21
     C                   EXSR      SRE021
     C*
     C     EMMSG#        WHENEQ    22
     C                   EXSR      SRE022
     C*
     C     EMMSG#        WHENEQ    23
     C                   EXSR      SRE023
     C*
     C     EMMSG#        WHENEQ    24
     C                   EXSR      SRE024
     C*
     C     EMMSG#        WHENEQ    25
     C                   EXSR      SRE025
     C*
     C     EMMSG#        WHENEQ    26
     C                   EXSR      SRE026
     C*
     C     EMMSG#        WHENEQ    27
     C                   EXSR      SRE027
     C*
     C     EMMSG#        WHENEQ    28
     C                   EXSR      SRE028
     C*
     C     EMMSG#        WHENEQ    29
     C                   EXSR      SRE029
     C*
     C     EMMSG#        WHENEQ    30
     C                   EXSR      SRE030
     C*
     C     EMMSG#        WHENEQ    31
     C                   EXSR      SRE031
     C*
     C     EMMSG#        WHENEQ    32
     C                   EXSR      SRE032
     C*
     C     EMMSG#        WHENEQ    33
     C                   EXSR      SRE033
     C*
     C     EMMSG#        WHENEQ    34
     C                   EXSR      SRE034
     C*
     C     EMMSG#        WHENEQ    35
     C                   EXSR      SRE035
     C*
     C     EMMSG#        WHENEQ    36
     C                   EXSR      SRE036
     C*
     C     EMMSG#        WHENEQ    37
     C                   EXSR      SRE037
     C*
     C     EMMSG#        WHENEQ    38
     C                   EXSR      SRE038
     C*
     C     EMMSG#        WHENEQ    39
     C                   EXSR      SRE039
     C*
     C     EMMSG#        WHENEQ    40
     C                   EXSR      SRE040
     C*
     C     EMMSG#        WHENEQ    41
     C                   EXSR      SRE041
     C*
     C     EMMSG#        WHENEQ    42
     C                   EXSR      SRE042
     C*
     C     EMMSG#        WHENEQ    43
     C                   EXSR      SRE043
     C*
     C     EMMSG#        WHENEQ    44
     C                   EXSR      SRE044
     C*
     C     EMMSG#        WHENEQ    45
     C                   EXSR      SRE045
     C*
     C     EMMSG#        WHENEQ    46
     C                   EXSR      SRE046
     C*
     C     EMMSG#        WHENEQ    47
     C                   EXSR      SRE047
     C*
     C     EMMSG#        WHENEQ    48
     C                   EXSR      SRE048
     C*
     C     EMMSG#        WHENEQ    49
     C                   EXSR      SRE049
     C*
     C     EMMSG#        WHENEQ    50
     C                   EXSR      SRE050
     C*
     C     EMMSG#        WHENEQ    51
     C                   EXSR      SRE051
     C*
     C     EMMSG#        WHENEQ    52
     C                   EXSR      SRE052
     C*
     C     EMMSG#        WHENEQ    53
     C                   EXSR      SRE053
     C*
     C     EMMSG#        WHENEQ    54
     C                   EXSR      SRE054
     C*
     C     EMMSG#        WHENEQ    55
     C                   EXSR      SRE055
     C*
     C     EMMSG#        WHENEQ    56
     C                   EXSR      SRE056
     C*
     C     EMMSG#        WHENEQ    57
     C                   EXSR      SRE057
     C*
     C     EMMSG#        WHENEQ    58
     C                   EXSR      SRE058
     C*
     C     EMMSG#        WHENEQ    59
     C                   EXSR      SRE059
     C*
     C     EMMSG#        WHENEQ    60
     C                   EXSR      SRE060
     C*
     C     EMMSG#        WHENEQ    61
     C                   EXSR      SRE061
     C*
     C     EMMSG#        WHENEQ    62
     C                   EXSR      SRE062
     C*
     C     EMMSG#        WHENEQ    63
     C                   EXSR      SRE063
     C*
     C     EMMSG#        WHENEQ    64
     C                   EXSR      SRE064
     C*
     C     EMMSG#        WHENEQ    65
     C                   EXSR      SRE065
     C*
     C     EMMSG#        WHENEQ    66
     C                   EXSR      SRE066
     C*
     C     EMMSG#        WHENEQ    67
     C                   EXSR      SRE067
     C*
     C     EMMSG#        WHENEQ    68
     C                   EXSR      SRE068
     C*
     C*** BEG ADD 10/25/05 ********************************
     C     EMMSG#        WHENEQ    112
     C                   EXSR      SRE112
     C*** END ADD 10/25/05 ********************************
     C*
     C                   OTHER
     C                   EXSR      SRE999
     C*
     C                   ENDSL
     C*
     C*
     C                   ENDIF
     C                   ENDIF
     C*** BEG ADD *** 06/14/07 ****************************
     C                   ENDIF
     C*** END ADD *** 06/14/07 ****************************
     C*
     C     EMKY          READE     PEM01                                  11
     C                   ENDDO
     C*
     C*   Return the error string to the caller.
     C*
     C*** BEGIN ADD *** 08/27/98 **************************************
     C*  End this program on the final call only to make this process
     C*    more efficient.
     C*
     C     XXDN#         IFEQ      100
     C*** END ADD ***** 08/27/98 **************************************
     C                   MOVE      '1'           *INLR
     C*** BEGIN ADD *** 08/27/98 **************************************
     C                   ENDIF
     C*
     C                   RETURN
     C*** END ADD ***** 08/27/98 **************************************
     C/EJECT
     C*================================================================
     C*          ROUTINE - SRINIT
     C*
     C*   Initiallization routine.
     C*          This routine ensures the following steps are done:
     C*          1. All work fields, parm lists, key lists, and
     C*             data structures are defined and initiallized.
     C*
     C     SRINIT        BEGSR
     C*
     C     *LIKE         DEFINE    QQCO#         XXCO#
     C     *LIKE         DEFINE    QQJST         XXJST
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXTKEY
     C     *LIKE         DEFINE    EMDN#         XXDN#
     C*** BEGIN ADD *** 08/27/98 **************************************
     C     *LIKE         DEFINE    LGTSET        XXTSET
     C*** BEG DEL ****** 03/24/10 *************************************
     C***        *LIKE     DEFN IXCSDT    XXCSDT
     C***        *LIKE     DEFN IXTRID    XXTRID
     C*** END DEL ****** 03/24/10 *************************************
     C*** END ADD ***** 08/27/98 **************************************
     C*
     C                   CLEAR                   XXERRS
     C*
     C                   MOVE      *BLANKS       XXDVAL           30
     C                   MOVE      *ZEROS        XTDAT8            8 0
     C                   MOVE      *ZEROS        XXDAT8            8 0
     C                   MOVE      *BLANKS       SRERR             1
     C                   MOVE      *BLANKS       XXST              2
     C                   MOVE      *BLANKS       XXWK1A            1
     C                   MOVE      *BLANKS       XXWK4A            4
     C*
     C*   Define parm lists
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3 0
     C                   PARM                    QQJST             2
     C                   PARM                    QQDN#             5 0
     C                   PARM                    QQSET             3
     C                   PARM                    QQMTC             2
     C                   PARM                    DN
     C                   PARM                    XXERRS           30
     c*** BEG ADD *** 07/02/13 **********************************
     c                   PARM                    QQFND             3
     c                   PARM                    QQFYR             3
     c                   PARM                    QQCASE            7
     c*** END ADD *** 07/02/13 **********************************
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
      *
     C*** BEG ADD ****** 03/24/10 *************************************
     C     IXAPI         PLIST
     C                   PARM                    XXCO#
     C                   PARM                    XXJST
     C                   PARM                    XXTRID            3
     C                   PARM                    CMFND
     C                   PARM      *BLANKS       @RTNCD            1
     C                   PARM      *BLANKS       @EDIF             3
     C                   PARM      *BLANKS       IXELT             1
     C                   PARM      *ZEROS        IXELTD            3 0
     C                   PARM      *BLANKS       IXFROI            2
     C                   PARM      *ZEROS        IXSDUE            3 0
     C                   PARM      *BLANKS       IXRLS             1
     C                   PARM      *ZEROS        IXCSDT            7 0
     C                   PARM      *BLANKS       IXEADC            5
     C                   PARM      *BLANKS       IXHCPR            5
     C                   PARM      *BLANKS       IXFNRC            5
     C                   PARM      *BLANKS       IXHCED            1
     C*** END ADD ****** 03/24/10 *************************************
      *
     C*
     C*   Define key lists
     C*
     C*          WTEML - Edit Matrix Table
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
     C                   KFLD                    XXDN#
     C*
     C*   KEY LIST FOR THE STATE TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY
     C*
     C*          WTABL - Work Comp Table File
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTKEY
     C*
     C*          WTGTL - Generic Table File
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXTKEY
     C*
     C*** BEGIN ADD *** 08/27/98 **************************************
     C*  Key list for the Log History file
     C*
     C     LGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXTSET
     C*
     C*  Key list for the EDI claim control file
     C*
     C     IXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
     C                   KFLD                    XXTRID
     C*** END ADD ***** 08/27/98 **************************************
     C*
     C*** Begin add *** 03/30/03 **************************************
     C*  Key list for the Claim Master file
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*** End add ***** 03/30/03 **************************************
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQJST         XXJST
     C                   MOVE      QQDN#         XXDN#
     C                   MOVE      QQDN#         P                 5 0
     C                   MOVE      *BLANK        XXKEY            10
     C*** BEGIN ADD *** 08/27/98 **************************************
     C                   MOVE      QQSET         XXTSET
     c*** BEG CHG *** 07/02/13 **********************************
     C*******            MOVEL     DN(15)        XXACLC
     C                   MOVEL     QQFND         XXFND
     C                   MOVEL     QQFYR         XXFYR
     C                   MOVEL     QQCASE        XXCASE
     c*** END CHG *** 07/02/13 **********************************
     C*
     C                   MOVE      '   '         XXTRID
     C                   MOVE      *BLANKS       XXCSDT
     C*** BEG CHG ****** 03/24/10 *************************************
     C***        IXKY      CHAINPIX01                60
     C*** BEG DEL ****** 08/11/10 *************************************
     C*****                CALL 'WA076R'  IXAPI        99
     C*****      @RTNCD    IFEQ 'Y'
     C*****                MOVE '0'       *IN60
     C*****                ELSE
     C*****                MOVE '1'       *IN60
     C*****                ENDIF
     C*****N60                MOVE IXCSDT    XXCSDT
     C**N60                MOVE IXCSDT    XXCSDT  70
     C*** END DEL ****** 08/11/10 *************************************
     C*** END CHG ****** 03/24/10 *************************************
     C*** END ADD ***** 08/27/98 **************************************
     C*
     C*** Begin add *** 03/30/03 **************************************
     C     CMKY          CHAIN     PCM01                              60
     C   60              CLEAR                   CMIDT
     C*** BEG ADD ****** 08/11/10 *************************************
     C                   CALL      'WA076R'      IXAPI                  99
     C     @RTNCD        IFEQ      'Y'
     C                   MOVE      '0'           *IN60
     C                   ELSE
     C                   MOVE      '1'           *IN60
     C                   ENDIF
     C  N60              MOVE      IXCSDT        XXCSDT            7 0
     C*** END ADD ****** 08/11/10 *************************************
     C*** End add ***** 03/30/03 **************************************
     C*** BEG ADD *** 01/27/06 *************************************
     C                   TIME                    DSDTTM
     C*** END ADD *** 01/27/06 *************************************
     C*
     C                   ENDSR
     C/EJECT
     C*** BEG ADD 01/27/06 ********************************
     C*=========================================================================
     C*   ROUTINE - SRCVDT
     C*
     C     SRCVDT        BEGSR
     C*
     C                   Z-ADD     XW1DT8        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         XXDAT8
     C                   Z-ADD     XW2DT8        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         XTDAT8
     C                   ENDSR
     C*** END ADD 01/27/06 ********************************
     C*=========================================================================
     C*   ROUTINE - SRE001
     C*
     C     SRE001        BEGSR
     C*
     C*** BEG ADD 01/27/06 ********************************
     C*
     C                   SELECT
     C* DN70
     C     P             WHENEQ    70
     C                   MOVEL     DN(85)        XX85              3
     C*
     C     XX85          IFEQ      '020'
     C     XX85          OREQ      '021'
     C     XX85          OREQ      '030'
     C     XX85          OREQ      '040'
     C     XX85          OREQ      '090'
     C*
     C     DN(P)         IFEQ      *BLANKS
     C     XXERRS        CAT       '001':0       XXERRS
     C                   ENDIF
     C                   ENDIF
     C* DN83 OR 84
     C     P             WHENEQ    83
     C     P             OREQ      84
     C                   MOVEL     DN(85)        XX85
     C                   MOVEL     DN(2)         XX02              2
     C*
     C     DN(86)        IFGT      *ZEROS
     C*
     C     XX02          IFEQ      'IP'
     C     XX02          OREQ      'AP'
     C     XX02          OREQ      'AB'
     C     XX02          OREQ      'CB'
     C     XX02          OREQ      'PY'
     C     XX02          OREQ      'FN'
     C     XX02          OREQ      '02'
     C     XX02          OREQ      'CO'
     C*
     C     XX85          IFEQ      '020'
     C     XX85          OREQ      '021'
     C     XX85          OREQ      '030'
     C     XX85          OREQ      '040'
     C     XX85          OREQ      '090'
     C*
     C     DN(P)         IFEQ      *BLANKS
     C     XXERRS        CAT       '001':0       XXERRS
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C* ALL OTHERS
     C     DN(P)         WHENEQ    *BLANKS
     C     XXERRS        CAT       '001':0       XXERRS
     C                   ENDSL
     C*** END ADD 01/27/06 ********************************
     C*** BEG DEL 01/27/06 ********************************
     C*******    DN,P      IFEQ *BLANKS
     C*******    XXERRS    CAT  '001':0   XXERRS
     C*******              ENDIF
     C*** END DEL 01/27/06 ********************************
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE002
     C*
     C     SRE002        BEGSR
     C*
     C                   MOVE      'TSET'        XXTBID
     C                   MOVEL(P)  DN(P)         XXTKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      '1'
     C     XXERRS        CAT       '002':0       XXERRS
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE003
     C*
     C     SRE003        BEGSR
     C*
     C     DN(1)         IFEQ      '148'
     C                   MOVE      'FMTC'        XXTBID
     C                   MOVEL(P)  DN(P)         XXTKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      '1'
     C     XXERRS        CAT       '003':0       XXERRS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE004
     C*
     C     SRE004        BEGSR
     C*
     C     DN(1)         IFEQ      'A49'
     C                   MOVE      'SMTC'        XXTBID
     C                   MOVEL(P)  DN(P)         XXTKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      '1'
     C     XXERRS        CAT       '004':0       XXERRS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE005
     C*
     C     SRE005        BEGSR
     C*
     C     DN(P)         IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXST
     C                   MOVEL     DN(P)         XXST
     C                   MOVEL(P)  XXST          XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      '1'
     C     XXERRS        CAT       '005':0       XXERRS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE006
     C*
     C     SRE006        BEGSR
     C*
     C     DN(P)         IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVEL     DN(P)         XXTKEY
     C     TBKY          CHAIN     PNA01                              12
     C     *IN12         IFEQ      *ON
     C     XXERRS        CAT       '006':0       XXERRS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE007
     C*
     C     SRE007        BEGSR
     C*
     C     DN(P)         IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVEL     DN(P)         XXTKEY
     C     TBKY          CHAIN     PPA01                              12
     C     *IN12         IFEQ      *ON
     C     XXERRS        CAT       '007':0       XXERRS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE008
     C*
     C     SRE008        BEGSR
     C*
     C     DN(P)         IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVEL     DN(P)         XXTKEY
     C     TBKY          CHAIN     PCA01                              12
     C     *IN12         IFEQ      *ON
     C     XXERRS        CAT       '008':0       XXERRS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE009
     C*
     C     SRE009        BEGSR
     C*
     C     DN(P)         IFNE      *BLANKS
     C                   MOVE      'GNDR'        XXTBID
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVEL     DN(P)         XXTKEY
     C     GTKY          CHAIN     PGT01                              12
     C     *IN12         IFEQ      *ON
     C     XXERRS        CAT       '009':0       XXERRS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE010
     C*
     C     SRE010        BEGSR
     C*
     C     DN(P)         IFNE      *BLANKS
     C                   MOVE      'MSTS'        XXTBID
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVEL     DN(P)         XXTKEY
     C     GTKY          CHAIN     PGT01                              12
     C     *IN12         IFEQ      *ON
     C     XXERRS        CAT       '010':0       XXERRS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE011
     C*
     C     SRE011        BEGSR
     C*
     C     DN(P)         IFNE      *BLANKS
     C     DN(62)        ORNE      *BLANKS                                      ADD 07/10/02
     C                   MOVE      'FRWP'        XXTBID
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVEL     DN(P)         XXTKEY
     C     GTKY          CHAIN     PGT01                              12
     C     *IN12         IFEQ      *ON
     C     XXERRS        CAT       '011':0       XXERRS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE012
     C*
     C     SRE012        BEGSR
     C*
     C     DN(P)         IFNE      *BLANKS
     C     DN(P)         ANDNE     'Y'
     C     DN(P)         ANDNE     'N'
     C     XXERRS        CAT       '012':0       XXERRS
     C                   END
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE013
     C*
     C     SRE013        BEGSR
     C*
     C     DN(P)         IFNE      *BLANKS
     C                   MOVE      'EMST'        XXTBID
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVEL     DN(P)         XXTKEY
     C     GTKY          CHAIN     PGT01                              12
     C     *IN12         IFEQ      *ON
     C     XXERRS        CAT       '013':0       XXERRS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE014
     C*
     C     SRE014        BEGSR
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE015
     C*
     C     SRE015        BEGSR
     C*
     C     DN(P)         IFNE      *BLANKS
     C                   MOVE      'SIC '        XXTBID
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVEL     DN(P)         XXTKEY
     C     GTKY          CHAIN     PGT01                              12
     C*** BEG ADD *** 09/14/07 ****************************
     C     *IN12         IFEQ      *ON
     C                   MOVE      'NAIC'        XXTBID
     C                   MOVEL(P)  DN(P)         XXTKEY
     C     GTKY          CHAIN     PGT01                              12
     C                   ENDIF
     C*** END ADD *** 09/14/07 ****************************
     C     *IN12         IFEQ      *ON
     C     XXERRS        CAT       '015':0       XXERRS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE016
     C*
     C     SRE016        BEGSR
     C*
     C     DN(P)         IFNE      *BLANKS
     C                   MOVE      'INIT'        XXTBID
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVEL     DN(P)         XXTKEY
     C     GTKY          CHAIN     PGT01                              12
     C     *IN12         IFEQ      *ON
     C     XXERRS        CAT       '016':0       XXERRS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE017
     C*
     C     SRE017        BEGSR
     C*
     C     DN(P)         IFNE      *BLANKS
     C     DN(P)         IFNE      'O'
     C     DN(P)         ANDNE     'C'
     C     DN(P)         ANDNE     'R'
     C     DN(P)         ANDNE     'X'
     C     XXERRS        CAT       '017':0       XXERRS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE018
     C*
     C     SRE018        BEGSR
     C*
     C     DN(P)         IFNE      *BLANKS
     C*** BEG CHG *** 01/27/06 ***************************
     C*******              MOVE DN,P      X       50
     C                   MOVEL     DN(P)         X                 5 0
     C*** END CHG *** 01/27/06 ***************************
     C     X             IFLT      0
     C     X             ORGT      7
     C     XXERRS        CAT       '018':0       XXERRS
     C                   ENDIF
     C*** BEG CHG *** 07/10/02 ***************************
     C                   ELSE
     C     DN(62)        IFNE      *BLANKS
     C********   DN,P      ANDEQ*BLANKS
     C     XXERRS        CAT       '018':0       XXERRS
     C                   ENDIF
     C*** END CHG *** 07/10/02 ***************************
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE019
     C*
     C     SRE019        BEGSR
     C*
     C     DN(P)         IFNE      *BLANKS
     C*** BEG CHG *** 01/27/06 ***************************
     C*******              MOVE DN,P      X
     C                   MOVEL     DN(P)         X
     C*** END CHG *** 01/27/06 ***************************
     C     X             IFLT      0
     C     X             ORGT      6
     C     XXERRS        CAT       '019':0       XXERRS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE020
     C*
     C     SRE020        BEGSR
     C*
     C     DN(P)         IFNE      *BLANKS
     C                   MOVE      'RTWQ'        XXTBID
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVEL     DN(P)         XXTKEY
     C     GTKY          CHAIN     PGT01                              12
     C     *IN12         IFEQ      *ON
     C     XXERRS        CAT       '020':0       XXERRS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE021
     C*
     C     SRE021        BEGSR
     C*
     C     DN(P)         IFNE      *BLANKS
     C     DN(P)         ANDNE     'M'
     C     DN(P)         ANDNE     'I'
     C     DN(P)         ANDNE     'N'
     C     DN(P)         ANDNE     'B'
     C     DN(P)         ANDNE     'L'
     C     DN(P)         ANDNE     'T'
     C     XXERRS        CAT       '021':0       XXERRS
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE022
     C*
     C     SRE022        BEGSR
     C*
     C     DN(P)         IFNE      *BLANKS
     C                   MOVE      'AGCP'        XXTBID
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVEL     DN(P)         XXTKEY
     C     GTKY          CHAIN     PGT01                              12
     C     *IN12         IFEQ      *ON
     C     XXERRS        CAT       '022':0       XXERRS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE023
     C*
     C     SRE023        BEGSR
     C*
     C     DN(P)         IFNE      *BLANKS
     C                   MOVE      'LRSN'        XXTBID
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVEL     DN(P)         XXTKEY
     C     GTKY          CHAIN     PGT01                              12
     C     *IN12         IFEQ      *ON
     C     XXERRS        CAT       '023':0       XXERRS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE024
     C*
     C     SRE024        BEGSR
     C*
     C     DN(P)         IFNE      *BLANKS
     C                   MOVE      'PADJ'        XXTBID
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVEL     DN(P)         XXTKEY
     C     GTKY          CHAIN     PGT01                              12
     C     *IN12         IFEQ      *ON
     C     XXERRS        CAT       '024':0       XXERRS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE025
     C*
     C     SRE025        BEGSR
     C*
     C     DN(P)         IFNE      *BLANKS
     C                   MOVE      'BADJ'        XXTBID
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVEL     DN(P)         XXTKEY
     C     GTKY          CHAIN     PGT01                              12
     C     *IN12         IFEQ      *ON
     C     XXERRS        CAT       '025':0       XXERRS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE026
     C*
     C     SRE026        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE027
     C*
     C     SRE027        BEGSR
     C*
     C                   MOVEL     DN(P)         XXWK1A
     C     XXWK1A        IFNE      *BLANK
     C     XXWK1A        ANDNE     '1'
     C     XXWK1A        ANDNE     '2'
     C     XXWK1A        ANDNE     '3'
     C     XXWK1A        ANDNE     '4'
     C     XXWK1A        ANDNE     '5'
     C     XXWK1A        ANDNE     '6'
     C     XXWK1A        ANDNE     '7'
     C     XXWK1A        ANDNE     '8'
     C     XXWK1A        ANDNE     '9'
     C     XXERRS        CAT       '027':0       XXERRS
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE028
     C*
     C     SRE028        BEGSR
     C*
     C                   MOVE      DN(P)         XXDVAL
     C     ' ':'0'       XLATE     XXDVAL        XXDVAL
     C                   TESTN                   XXDVAL               12
     C     *IN12         IFEQ      *OFF
     C     XXERRS        CAT       '028':0       XXERRS
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE029
     C*
     C     SRE029        BEGSR
     C*
     C                   MOVE      DN(P)         XXDAT8
     C     XXDAT8        IFNE      0
     C*
     C                   MOVE      XXDAT8        XXWK4A
     C                   MOVEL     XXWK4A        XTDAT8
     C                   MOVEL     XXDAT8        XXWK4A
     C                   MOVE      XXWK4A        XTDAT8
     C                   Z-ADD     XTDAT8        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XTDAT8
     C     SRERR         IFNE      ' '
     C                   MOVE      '1'           *IN12
     C                   ENDIF
     C*
     C     *IN12         IFEQ      *ON
     C     XXERRS        CAT       '029':0       XXERRS
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE030
     C*
     C     SRE030        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE031
     C*
     C     SRE031        BEGSR
     C*
     C                   MOVE      DN(P)         XXTIM4
     C     XXTIM4        IFGT      2400
     C     XXTIM4        ORLT      0
     C     XXMM          ORLT      0
     C     XXMM          ORGT      59
     C     XXERRS        CAT       '031':0       XXERRS
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE032
     C*
     C     SRE032        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE033
     C*
     C     SRE033        BEGSR
     C*
     C*** BEG CHG *** 01/27/06 ***************************
     C*******              MOVE DN,P      XXDAT8
     C*******              MOVE DN,31     XTDAT8
     C                   MOVEL(P)  DN(P)         XW1DT8            8 0
     C                   MOVEL(P)  DN(31)        XW2DT8            8 0
     C*
     C                   EXSR      SRCVDT
     C*** END CHG *** 01/27/06 ***************************
     C     XXDAT8        IFNE      0
     C     XTDAT8        ANDNE     0
     C     XXDAT8        IFGT      XTDAT8
     C     XXERRS        CAT       '033':0       XXERRS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE034
     C*
     C     SRE034        BEGSR
     C*
     C*** BEG CHG *** 01/27/06 ***************************
     C*******              MOVE DN,P      XXDAT8
     C*******              MOVE DN,31     XTDAT8
     C                   MOVEL(P)  DN(P)         XW1DT8
     C                   MOVEL(P)  DN(31)        XW2DT8
     C*
     C                   EXSR      SRCVDT
     C*** END CHG *** 01/27/06 ***************************
     C     XXDAT8        IFNE      0
     C     XTDAT8        ANDNE     0
     C     XXDAT8        IFLT      XTDAT8
     C     XXERRS        CAT       '034':0       XXERRS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE035
     C*
     C     SRE035        BEGSR
     C*
     C*** BEG CHG *** 01/27/06 ***************************
     C*******              MOVE DN,P      XXDAT8
     C*******              MOVE DN,56     XTDAT8
     C                   MOVEL(P)  DN(P)         XW1DT8
     C                   MOVEL(P)  DN(56)        XW2DT8
     C*
     C                   EXSR      SRCVDT
     C*** END CHG *** 01/27/06 ***************************
     C     XXDAT8        IFNE      0
     C     XTDAT8        ANDNE     0
     C     XXDAT8        IFLT      XTDAT8
     C     XXERRS        CAT       '035':0       XXERRS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE036
     C*
     C     SRE036        BEGSR
     C*
     C*** BEG CHG *** 01/27/06 ***************************
     C*******              MOVE DN,P      XXDAT8
     C*******              MOVE DN,57     XTDAT8
     C                   MOVEL(P)  DN(P)         XW1DT8
     C                   MOVEL(P)  DN(57)        XW2DT8
     C*
     C                   EXSR      SRCVDT
     C*** END CHG *** 01/27/06 ***************************
     C     XXDAT8        IFNE      0
     C     XTDAT8        ANDNE     0
     C     XXDAT8        IFGT      XTDAT8
     C     XXERRS        CAT       '036':0       XXERRS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE037
     C*
     C     SRE037        BEGSR
     C*
     C*** BEG CHG *** 01/27/06 ***************************
     C*******              MOVE DN,P      XXDAT8
     C*******              MOVE DN,3      XTDAT8
     C                   MOVEL(P)  DN(P)         XW1DT8
     C                   MOVEL(P)  DN(3)         XW2DT8
     C*
     C                   EXSR      SRCVDT
     C*** END CHG *** 01/27/06 ***************************
     C     XXDAT8        IFNE      0
     C     XTDAT8        ANDNE     0
     C     XXDAT8        IFGT      XTDAT8
     C     XXERRS        CAT       '037':0       XXERRS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE038
     C*
     C     SRE038        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE039
     C*
     C     SRE039        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE040
     C*
     C     SRE040        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE041
     C*
     C     SRE041        BEGSR
     C*
     C*** BEG CHG *** 01/27/06 ***************************
     C*******              MOVE DN,P      XXDAT8
     C                   MOVEL     DN(P)         XWDAT8            8 0
     C                   Z-ADD     XWDAT8        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         XXDAT8
     C*****************    Z-ADDUDATE     SRDTI
     C                   Z-ADD     DSDATE        SRDTI
     C*** END CHG *** 01/27/06 ***************************
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         XTDAT8
     C     XXDAT8        IFNE      0
     C     XTDAT8        ANDNE     0
     C     XXDAT8        IFGT      XTDAT8
     C     XXERRS        CAT       '041':0       XXERRS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE042
     C*
     C     SRE042        BEGSR
     C*
     C*** Begin add *** 03/08/04 **************************************
     C*  If jurisdiction state = 'PA', either the 'Date Lasted Worked'
     C*  or the 'RTW Date' must exist.
     C*
     C     QQJST         IFEQ      'PA'
     C     DN(65)        ANDEQ     *BLANKS
     C     DN(68)        ANDEQ     *BLANKS
     C     XXERRS        CAT       '042':0       XXERRS
     C                   ENDIF
     C*** End add ***** 03/08/04 **************************************
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE043
     C*
     C     SRE043        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE044
     C*
     C     SRE044        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE045
     C*
     C     SRE045        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE046
     C*
     C     SRE046        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE047
     C*
     C     SRE047        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE048
     C*
     C     SRE048        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE049
     C*
     C     SRE049        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE050
     C*
     C     SRE050        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE051
     C*
     C     SRE051        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE052
     C*
     C     SRE052        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE053
     C*
     C     SRE053        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE054
     C*
     C     SRE054        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE055
     C*
     C     SRE055        BEGSR
     C*
     C*** BEG CHG *** 01/27/06 ***************************
     C*******              MOVE DN,P      XXDAT8
     C*******              MOVE DN,61     XTDAT8
     C                   MOVEL(P)  DN(P)         XW1DT8
     C                   MOVEL(P)  DN(61)        XW2DT8
     C*
     C                   EXSR      SRCVDT
     C*** END CHG *** 01/27/06 ***************************
     C     XXDAT8        IFNE      0
     C     XTDAT8        ANDNE     0
     C     XXDAT8        IFGT      XTDAT8
     C     XXERRS        CAT       '055':0       XXERRS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE056
     C*
     C     SRE056        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE057
     C*
     C     SRE057        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE058
     C*
     C     SRE058        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE059
     C*
     C     SRE059        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE060
     C*
     C     SRE060        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE061
     C*
     C     SRE061        BEGSR
     C*
     C*  If jurisdiction state = 'FL', the 148 '00' and A49 'IP' or 'FS'
     C*  must be extracted on the same date.
     C*
     C     QQJST         IFEQ      'FL'
     C*
     C                   OPEN      WMLGL4
     C                   SELECT
     C*
     C     QQSET         WHENEQ    '148'
     C     QQMTC         ANDEQ     '00'
     C*
     C                   MOVE      'A49'         XXTSET
     C     LGKY          CHAIN     PLG01                              60
     C     *IN60         DOWEQ     *OFF
     C     LGMTC         IFEQ      'IP'
     C     LGSTS         ANDNE     'TR'
     C     LGMTC         OREQ      'FS'
     C     LGSTS         ANDNE     'TR'
     C*** BEG ADD *** 02/04/08 ***************************
     C     LGMTC         OREQ      'PY'
     C     LGSTS         ANDNE     'TR'
     C     LGMTC         OREQ      'EP'
     C     LGSTS         ANDNE     'TR'
     C     LGMTC         OREQ      'PD'
     C     LGSTS         ANDNE     'TR'
     C     LGMTC         OREQ      'AP'
     C     LGSTS         ANDNE     'TR'
     C     LGMTC         OREQ      'CD'
     C     LGSTS         ANDNE     'TR'
     C     LGMTC         OREQ      'VE'
     C     LGSTS         ANDNE     'TR'
     C*** END ADD *** 02/04/08 ***************************
     C                   LEAVE
     C                   ENDIF
     C     LGKY          READE     PLG01                                  60
     C                   ENDDO
     C*
     C     QQSET         WHENEQ    'A49'
     C     QQMTC         IFEQ      'IP'
     C     QQMTC         OREQ      'FS'
     C*** BEG ADD *** 02/04/08 ***************************
     C     QQMTC         OREQ      'PY'
     C     QQMTC         OREQ      'EP'
     C     QQMTC         OREQ      'PD'
     C     QQMTC         OREQ      'AP'
     C     QQMTC         OREQ      'CD'
     C     QQMTC         OREQ      'VE'
     C*** END ADD *** 02/04/08 ***************************
     C*
     C                   MOVE      '148'         XXTSET
     C     LGKY          CHAIN     PLG01                              60
     C     *IN60         DOWEQ     *OFF
     C     LGMTC         IFEQ      '00'
     C     LGSTS         IFNE      'EC'
     C     LGSTS         ANDNE     'EI'
     C     LGSTS         ANDNE     'TR'
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C     LGKY          READE     PLG01                                  60
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C                   ENDSL
     C                   CLOSE     WMLGL4
     C                   MOVE      QQSET         XXTSET
     C*
     C     *IN60         IFEQ      *ON
     C     XXERRS        CAT       '061':0       XXERRS
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE062
     C*
     C     SRE062        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE063
     C*
     C     SRE063        BEGSR
     C*
     C*** BEGIN ADD *** 08/27/98 **************************************
     C*  An 'IP' or 'FS' must be accepted to submit this transaction.
     C*
     C                   OPEN      WMLGL4
     C*
     C     LGKY          CHAIN     PLG01                              60
     C     *IN60         DOWEQ     *OFF
     C     LGMTC         IFEQ      'IP'
     C     LGMTC         OREQ      'FS'
     C*** Begin add *** 03/30/03 **************************************
     C     LGMTC         OREQ      '04'
     C     LGMTC         OREQ      'AP'
     C*** End add ***** 03/30/03 **************************************
     C*** Begin add *** 04/10/17 **************************************
     C     LGMTC         OREQ      'EP'
     C*** End add ***** 04/10/17 **************************************
     C     LGSTS         IFEQ      'TA'
     C     LGSTS         OREQ      'TE'
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C     LGKY          READE     PLG01                                  60
     C                   ENDDO
     C*
     C                   CLOSE     WMLGL4
     C*
     C     *IN60         IFEQ      *ON
     C*** Begin add *** 03/30/03 **************************************
     C*** Begin chg *** 05/06/03 **************************************
     C********** CMIDT     ORGE XXCSDT
     C     CMIDT         ANDGE     XXCSDT
     C*** End chg ***** 05/06/03 **************************************
     C*** End add ***** 03/30/03 **************************************
     C     XXERRS        CAT       '063':0       XXERRS
     C                   ENDIF
     C*** END ADD ***** 08/27/98 **************************************
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE064
     C*
     C     SRE064        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE065
     C*
     C     SRE065        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE066
     C*
     C     SRE066        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE067
     C*
     C     SRE067        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE068
     C*
     C     SRE068        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE069
     C*
     C     SRE069        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE070
     C*
     C     SRE070        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE071
     C*
     C     SRE071        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE072
     C*
     C     SRE072        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE073
     C*
     C     SRE073        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE074
     C*
     C     SRE074        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE075
     C*
     C     SRE075        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE076
     C*
     C     SRE076        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE077
     C*
     C     SRE077        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE078
     C*
     C     SRE078        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE079
     C*
     C     SRE079        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE080
     C*
     C     SRE080        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE081
     C*
     C     SRE081        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE082
     C*
     C     SRE082        BEGSR
     C*
     C*
     C                   ENDSR
     C*** BEG ADD 10/25/05 ********************************
     C*
     C*   ROUTINE - SRE112
     C*
     C     SRE112        BEGSR
     C*
     C*** BEG CHG *** 01/27/06 ***************************
     C*******              MOVELDN,P      XXDAT8
     C*******              MOVELDN,65     XTDAT8
     C                   MOVEL(P)  DN(P)         XW1DT8
     C                   MOVEL(P)  DN(65)        XW2DT8
     C*
     C                   EXSR      SRCVDT
     C*** END CHG *** 01/27/06 ***************************
     C     XXDAT8        IFNE      0
     C     XTDAT8        ORNE      0
     C     XXDAT8        IFLT      XTDAT8
     C     XXERRS        CAT       '112':0       XXERRS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END ADD 10/25/05 ********************************
     C*
     C*   ROUTINE - SRE998
     C*
     C     SRE998        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE999
     C*
     C     SRE999        BEGSR
     C*
     C*
     C                   ENDSR
