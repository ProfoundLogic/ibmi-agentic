     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE WA724R - Calculate Lost Days on Claim
     H*****************************************************************
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W8904R - CI Center Extract
     H*
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                   *
     H*                                                               *
     H* 01/20/10 C:61 Support: WA093 69099                            *
     H*               Copied from W0576R and modified to be API       *
     H*               to return lost days on a claim.                 *
     H*                                                               *
     H*****************************************************************
      *
      *  Payment Detail Physical
     FWDPAL     IF   E           K DISK
      *
      * NEW ARRAYS ADDED TO HANDLE SERVICE PERIOD OVERLAPPING

     D FRDT            S              7  0 DIM(9999) ASCEND                     DATE ARRAY
     D FRTO            S             14    DIM(9999) ASCEND                     DATE ARRAY
      *
      *
      *================================================================
      *  /COPY required constants
      *================================================================
     D XXHIVA          C                   CONST('99999999999999')
      *
      *
      *================================================================
      *  PLIST definitions
      *================================================================
      *

     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@LOST            4 0          LOST DAYS
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
      *
      *
      *================================================================
      *  KLIST definitions
      *================================================================
      *
      *  Payment Detail Key
      *
     C     PAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
      *
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
      *
     C     *LIKE         DEFINE    PACO#         KKCO#
     C     *LIKE         DEFINE    PAFND         KKFND
     C     *LIKE         DEFINE    PAFYR         KKFYR
     C     *LIKE         DEFINE    PACASE        KKCASE
      *
      *
      *==============================================================
      *  BEGIN
      *==============================================================
      *
      * Process Claim Payment Detail records
      *
     C                   Z-ADD     *ZERO         XXWKS
     C                   Z-ADD     *ZERO         XXDYS
     C                   Z-ADD     0             T                 4 0
     C                   CLEAR                   FRDT
     C                   CLEAR                   FRTO
     C                   MOVEA     *HIVAL        FRDT
     C                   MOVE      XXHIVA        FRTO
      *
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@FND         KKFND
     C                   MOVE      @@FYR         KKFYR
     C                   MOVE      @@CASE        KKCASE
      *
     C     PAKY          CHAIN     PPA01                              60
      *
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      CUMM
     C     PAKY          READE     PPA01                                  60
     C                   ENDDO
      *
      *  Assign Screen Fields
      *
     C     7             MULT      XXWKS         @@LOST
     C                   ADD       XXDYS         @@LOST
      *
     C                   MOVE      *ON           *INLR
      *
      *
      *===============================================================
      * CUMM - Subroutine to Accumulate Total Payment Amount and
      *        Total Weeks and Days
      *===============================================================
      *
     C     CUMM          BEGSR
      *
      *  WEEKS & DAYS CALCULATION
      *
      ** Ignore records with PAADST <> *BLANKS, VOIDS AND STOPS
      *
     C     PAADST        IFEQ      *BLANKS
     C     PATYPE        ANDNE     'V'
     C     PATYPE        ANDNE     'S'
      *
     C     PAWKS         IFGT      0
     C     PADAYS        ORGT      0
      *
      * Determine if this record to be considered for calculation
      * If service dates of current record are within existing service dates
      * then it is overlapping record, ignore
      *
     C                   Z-ADD     0             XXDYFT            3 0
      **
     C                   SORTA     FRDT
     C                   SORTA     FRTO
      **
     C                   Z-ADD     1             R                 4 0
     C                   Z-ADD     1             K                 4 0
     C     PAFRDT        LOOKUP    FRDT(R)                              8080
     C     PATODT        LOOKUP    FRDT(K)                              8181
      *
      ** Get TO date for period R and K
      *
     C     *IN80         IFEQ      *ON
     C                   MOVE      FRTO(R)       XXTOR             7 0
     C                   ENDIF
      *
     C     *IN81         IFEQ      *ON
     C                   MOVE      FRTO(K)       XXTOK             7 0
     C                   ENDIF
      **
     C     *IN80         IFEQ      *OFF
     C     *IN81         ANDEQ     *OFF
      **
      ** CASE 1:
      ** No array record found, not an overlapping period.
      ** Save period into array for further processing
      **
     C                   MOVE      'N'           XXIGNR            1
     C                   ADD       1             T
     C                   Z-ADD     PAFRDT        FRDT(T)
     C                   MOVEL     PAFRDT        FRTO(T)
     C                   MOVE      PATODT        FRTO(T)
     C                   ENDIF                                                  *IN80/81 = *OFF
      **
     C     *IN80         IFEQ      *OFF
     C     *IN81         ANDEQ     *ON
     C                   MOVE      'N'           XXIGNR
      **
      ** CASE 2: PAFRDT is not found, PATODT is found in K period
      **    Get Days diff between PAFRDT and FRDT(1)
      **
     C                   Z-ADD     PAFRDT        XXFDT             7 0
     C                   Z-ADD     FRDT(1)       XXTDT             7 0
     C                   EXSR      SRDTDF
     C                   ADD       QQDAYS        XXDYFT
     C                   Z-ADD     PAFRDT        FRDT(1)
     C                   MOVEL     PAFRDT        FRTO(1)
      **
      **  There may be more than 1 period between 2 and K, get days diff between
      **  all periods, avoiding overlaps :get days between this period TODT and
      **  and next period FRDT then close gap by setting FRDT nxt = TODT cur
      **
     C     2             DO        K             X                11 0
      ** Calc difference between TODT and FRDT of next period
     C     X             SUB       1             Y                11 0
      ** Set period dates to no diff
     C                   MOVE      FRTO(Y)       XXFDT                          moving TODT
     C                   Z-ADD     FRDT(X)       XXTDT
     C                   EXSR      SRDTDF
     C                   ADD       QQDAYS        XXDYFT
     C                   MOVE      FRDT(X)       FRTO(Y)                        moving TODT
     C                   ENDDO                                                  2 to K
      **
     C     PATODT        IFGT      XXTOK
      **
      ** CASE 3: PATODT is beyond K period to date
      ** Get Days diff between TODT(K) and PATODT
      **
     C                   Z-ADD     XXTOK         XXFDT
     C                   Z-ADD     PATODT        XXTDT
     C                   EXSR      SRDTDF
     C                   ADD       QQDAYS        XXDYFT
     C                   MOVE      PATODT        FRTO(K)
     C                   ENDIF                                                  CASE 2B
      **
     C                   ENDIF                                                  *IN80=*OFF, 81=*ON
      **
      ** Found array from date and To Date
      **
     C     *IN80         IFEQ      *ON
     C     *IN81         ANDEQ     *ON
      **
      ** PAFRDT and PATODT found in same period
     C     R             IFEQ      K
      **
      ** CASE 4:
      ** Period starts within existing period and TO date is also within
      ** existing period
      **
     C     PAFRDT        IFLE      XXTOR
     C     PATODT        IFLE      XXTOR
      ** This is pure overlap record, IGNORE completely
     C                   MOVE      'Y'           XXIGNR
      **
     C                   ELSE                                                   PATODT<=XXTOR
      **
      ** CASE 5:
      ** PATODT is greater than XXTOR
      ** Get Days diff between XXTOR and PATODT
     C                   MOVE      'N'           XXIGNR
     C                   Z-ADD     XXTOR         XXFDT
     C                   Z-ADD     PATODT        XXTDT
     C                   EXSR      SRDTDF
     C                   ADD       QQDAYS        XXDYFT
     C                   MOVE      PATODT        FRTO(R)
     C                   ENDIF                                                  PATODT<=XXTOR
      **
     C                   ELSE                                                   PAFRDT<=XXTOR
      **
      ** CASE 6:
      ** PAFRDT is greater than XXTOR (FR and TO both hit same period)
      ** This is a new period in between add entry
      ** Save period into array for further processing
     C                   MOVE      'N'           XXIGNR
     C                   ADD       1             T
     C                   Z-ADD     PAFRDT        FRDT(T)
     C                   MOVEL     PAFRDT        FRTO(T)
     C                   MOVE      PATODT        FRTO(T)
     C                   ENDIF                                                  PAFRDT<=XXTOR
      **
     C                   ELSE                                                   R = K
      **
      ** FRDT and TODT hit different periods
      **  From date beyond arry To date: This period starts after array period
      **
     C     PAFRDT        IFGT      XXTOR
      **
      ** CASE 7:
      **  TO date lies in K period, calculate days between these periods
      ** Get Days diff between PAFRDT and FRDT(R+1)
     C                   Z-ADD     PAFRDT        XXFDT
     C                   ELSE                                                   PAFRDT>XXTOR
      **
      ** CASE 8:
      **  TO date lies in K period, calculate days between these periods
      ** Get Days diff between TODT and FRDT,R+1
     C                   MOVE      FRTO(R)       XXFDT
     C                   ENDIF                                                  PAFRDT>XXTOR
      **
     C                   MOVE      'N'           XXIGNR
      **
      ** Common process for case 7 and 8
     C     R             ADD       1             Z                 4 0
     C                   Z-ADD     FRDT(Z)       XXTDT
     C                   EXSR      SRDTDF
     C                   ADD       QQDAYS        XXDYFT
      **  There may be more than 1 period between Z and K, get days diff between
      **  all periods, avoiding overlaps :get days between this period TODT and
      **  and next period FRDT then close gap by setting FRDT nxt = TODT cur
     C     Z             DO        K             X
      ** Cal difference between TODT and FRDT of next period
     C     X             ADD       1             Y
      ** Set period dates to no diff
     C                   MOVE      FRTO(X)       XXFDT
     C                   Z-ADD     FRDT(Y)       XXTDT
     C                   EXSR      SRDTDF
     C                   ADD       QQDAYS        XXDYFT
     C                   MOVE      FRTO(X)       FRDT(Y)
     C                   MOVEL     FRDT(Y)       FRTO(Y)
     C                   ENDDO                                                  R to K
      **
      ** CASE 9:
      **  TO date lies outside K period, calculate days outside K period and
      **  and add to days calculated in CASE 3D
     C     PATODT        IFGT      XXTOK
     C                   Z-ADD     XXTOK         XXFDT
     C                   Z-ADD     PATODT        XXTDT
     C                   EXSR      SRDTDF
     C                   ADD       QQDAYS        XXDYFT
      ** Set period dates to no diff
     C                   MOVE      PATODT        FRTO(K)
     C                   ENDIF                                                  PATODT > XXTOK
      **
     C                   ENDIF                                                  R=K
     C                   ENDIF                                                  80/81 = *ON
      **
      ** Override ignore flag if adjustment/reversal/void
     C     PATYPE        IFEQ      'A'
     C     PAAMT         ANDLT     0
     C     PATYPE        OREQ      'R'
     C     PAAMT         ANDGT     0
     C                   MOVE      'N'           XXIGNR
     C                   ENDIF
     C*
      **
      ** If XXIGNR flag set to Y then ignore this record for days calculation
     C     XXIGNR        IFEQ      'N'
     C                   MOVE      'N'           XXADD             1
      *
     C     PATYPE        IFEQ      'P'
     C     PAAMT         ANDGT     0
     C     PATYPE        OREQ      'M'
     C     PAAMT         ANDGT     0
     C     PATYPE        OREQ      'A'
     C     PAAMT         ANDGT     0
     C     PATYPE        OREQ      'Z'
     C     PAAMT         ANDGT     0
     C     PATYPE        OREQ      'R'
     C     PAAMT         ANDLT     0
     C     PATYPE        OREQ      'S'
     C     PAAMT         ANDLT     0
     C     PATYPE        OREQ      'V'
     C     PAAMT         ANDLT     0
     C                   MOVE      'Y'           XXADD
     C                   ENDIF
      *
     C     XXADD         IFEQ      'Y'
      *                                                                    ****
     C     XXDYFT        IFEQ      0
     C                   ADD       PAWKS         XXWKS             3 0
     C                   ADD       PADAYS        XXDYS             3 0
      *                                                                    ****
     C                   ELSE
      *                                                                    ****
     C     XXDYFT        DIV       7             XXTWKS            3 0
     C                   MVR                     XXDYFT
     C                   ADD       XXTWKS        XXWKS
     C                   ADD       XXDYFT        XXDYS
     C                   ENDIF                                                  XXDYFT = 0
      *
     C     XXDYS         IFGE      7
     C                   ADD       1             XXWKS
     C     XXDYS         SUB       7             XXDYS
     C                   ENDIF
     C                   ENDIF
      *
     C     XXADD         IFEQ      'N'
      *                                                                    ****
     C     XXDYFT        IFEQ      0
     C                   SUB       PAWKS         XXWKS
     C     XXDYS         SUB       PADAYS        XXDYS
      *                                                                    ****
     C                   ELSE
      *                                                                    ****
     C     XXDYFT        DIV       7             XXTWKS
     C                   MVR                     XXDYFT
     C                   SUB       XXTWKS        XXWKS
     C                   SUB       XXDYFT        XXDYS
     C                   ENDIF
      *                                                                    ****
     C                   SELECT
      *                                                                    ****
     C     XXDYS         WHENLT    0
     C     XXWKS         ANDGT     0
     C     XXDYS         ORLE      -7
     C                   SUB       1             XXWKS
     C     7             ADD       XXDYS         XXDYS
      *                                                                    ****
     C     XXWKS         WHENLT    0
     C     XXDYS         ANDGT     0
     C                   ADD       1             XXWKS
     C                   SUB       7             XXDYS
      *                                                                    ****
     C                   ENDSL
      *
     C                   ENDIF
      *                                                                    *****
     C                   ENDIF                                                  XXIGNR = 'N'
      *                                                                    *****
     C                   ENDIF
      *                                                                    *****
     C                   ENDIF                                                  PAADST = ' '
      *
     C                   ENDSR                                                  CUMM
      *
      *
      *
      *================================================================
      * SRDTDF - Calculate number of days between two dates
      *================================================================
     C     SRDTDF        BEGSR
      *
     C                   Z-ADD     XXFDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQFROM            8 0
      *
     C                   Z-ADD     XXTDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQTO              8 0
      *
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   MOVE      *BLANK        QQERR             1
     C                   MOVE      '1'           QQTERM            1
      *
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
      *                                                                    *
     C                   ENDSR
      *                                                                    *
      *                                                                    *
