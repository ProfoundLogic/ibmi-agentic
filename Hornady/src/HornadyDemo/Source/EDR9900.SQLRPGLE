     H COPYRIGHT('(C) Copyright Midrange Computer Associates, Inc., 2007')
     H nomain
      **************************************************************************
      * APPLICATION: EDI                                                       *
      *       TITLE: EDI Subprocedures                                         *
      *          BY: William Earl (MCA)                                        *
      *        DATE: August 2007                                               *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------- *
      *  11/27/2007  MCA  Check trading partner cross reference file when      *
      *                   accessing 850 data.                                  *
      *  05/28/2008  MCA  Add RtvParentPO subprocedure.                        *
      *  09/08/2008  MCA  Add access to REF segment data.                      *
      *  09/17/2010  MCA  Add access to REF03 data element.                    *
      *  08/18/2014  MCA  Specifically point EDLTPXR1 to library EDISAVE.      *
      *                   If shipping program (HYR0110) call to get EDI data   *
      *                   (specifically Dick's) did not find the purchase      *
      *                   order requested, file EDLTPXR1 was not found.        *
      *  09/09/2020  MCA  Fix bug - when retrieving 850 N1 or REF data, if the *
      *                   customer's partner ID has been changed, when         *
      *                   retrieving based on the old partner ID, only the     *
      *                   last REF segment was being looked at.                *
      *  01/02/2021  MCA  Temporary change (for about a year or so) to check   *
      *                   for the PO number in BEG04, and if found, use that   *
      *                   PO to retrieve the 850 record.  This was done due to *
      *                   Dick's EDI trading partner ID change in August 2020. *
      *                   See notes inline with the code.  This also required  *
      *                   changing this module type from RPGLE to SQLRPGLE.    *
      *                                                                        *
      **************************************************************************

     FHMCMLF02  IF   E           K DISK    EXTFILE('EDISAVE/HMCMLF02') USROPN
     FEDLTPXR1  IF   E           K DISK    EXTFILE('EDISAVE/EDLTPXR1') USROPN
     FHMCEIPBEG2IF   E           K DISK    USROPN RENAME(EIPBEGR:EIPBEGR2)

      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
      /COPY *LIBL/QPRPSRC,EDR9900

     D RtvRcd          PR
     D  SegmentID                     3A   CONST
     D  SenderID                     15A   CONST
     D  PurOrd#                      22A   CONST
     D  Qualifier                    10A   CONST OPTIONS(*NOPASS:*VARSIZE)

     P/EJECT
      **************************************************************************
      * Retrieve a character field from EDI 850 data.                          *
      * Return a pointer to the data.                                          *
      **************************************************************************
     P EDI850Char      B                   EXPORT

     D                 PI              *
     D SenderID                      15A   CONST
     D PurOrd#                       22A   CONST
     D FldName                       10A   CONST
     D Qualifier                     10A   CONST OPTIONS(*NOPASS:*VARSIZE)

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_SenderID     S             15A
     D wk_PurOrd#      S             22A
     D wk_FldName      S             10A
     D wk_Qualifier    S             10A
     D wk_Data         S            256A   STATIC
     D wk_DataPtr      S               *

      *
      * Retrieve the character field data.
      *
     C                   EVAL      wk_SenderID = SenderID
     C                   EVAL      wk_PurOrd# = PurOrd#
     C                   EVAL      wk_FldName = FldName
     C                   IF        %parms > 3
     C                   EVAL      wk_Qualifier = Qualifier
     C                   ELSE
     C                   EVAL      wk_Qualifier = *Blanks
     C                   ENDIF                                                  %parms>3
     C                   EVAL      wk_Data = *Blanks

     C                   SELECT
     C                   WHEN      wk_FldName = 'BEG01'
     C                   CALLP     RtvRcd('BEG':wk_SenderID:wk_PurOrd#)
     C                   EVAL      wk_Data = BEG01
     C                   WHEN      wk_FldName = 'BEG02'
     C                   CALLP     RtvRcd('BEG':wk_SenderID:wk_PurOrd#)
     C                   EVAL      wk_Data = BEG02
     C                   WHEN      wk_FldName = 'BEG04'
     C                   CALLP     RtvRcd('BEG':wk_SenderID:wk_PurOrd#)
     C                   EVAL      wk_Data = BEG04
     C                   WHEN      wk_FldName = 'N102'
     C                   CALLP     RtvRcd('N1':wk_SenderID:wk_PurOrd#:
     C                               wk_Qualifier)
     C                   EVAL      wk_Data = N102
     C                   WHEN      wk_FldName = 'N103'
     C                   CALLP     RtvRcd('N1':wk_SenderID:wk_PurOrd#:
     C                               wk_Qualifier)
     C                   EVAL      wk_Data = N103
     C                   WHEN      wk_FldName = 'N104'
     C                   CALLP     RtvRcd('N1':wk_SenderID:wk_PurOrd#:
     C                               wk_Qualifier)
     C                   EVAL      wk_Data = N104
     C                   WHEN      wk_FldName = 'REF02'
     C                   CALLP     RtvRcd('REF':wk_SenderID:wk_PurOrd#:
     C                               wk_Qualifier)
     C                   EVAL      wk_Data = REF02
     C                   WHEN      wk_FldName = 'REF03'
     C                   CALLP     RtvRcd('REF':wk_SenderID:wk_PurOrd#:
     C                               wk_Qualifier)
     C                   EVAL      wk_Data = REF03
     C                   ENDSL

      *
      * Return the pointer to the data.
      *
     C                   EVAL      wk_DataPtr = %addr(wk_Data)
     C                   RETURN    wk_DataPtr

     P EDI850Char      E

     P/EJECT
      **************************************************************************
      * Retrieve a numeric field from EDI 850 data.                            *
      * Return a pointer to the data.                                          *
      **************************************************************************
     P EDI850Nbr       B                   EXPORT

     D                 PI              *
     D SenderID                      15A   CONST
     D PurOrd#                       22A   CONST
     D FldName                       10A   CONST
     D Qualifier                     10A   CONST OPTIONS(*NOPASS:*VARSIZE)

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_SenderID     S             15A
     D wk_PurOrd#      S             22A
     D wk_FldName      S             10A
     D wk_Qualifier    S             10A
     D wk_Data         S            256A   STATIC
     D wk_DataPtr      S               *

      *
      * Retrieve the numeric field data.
      *
     C                   EVAL      wk_SenderID = SenderID
     C                   EVAL      wk_PurOrd# = PurOrd#
     C                   EVAL      wk_FldName = FldName
     C                   IF        %parms > 3
     C                   EVAL      wk_Qualifier = Qualifier
     C                   ELSE
     C                   EVAL      wk_Qualifier = *Blanks
     C                   ENDIF                                                  %parms>3
     C                   EVAL      wk_Data = *Zeros

     C                   SELECT
     C                   WHEN      wk_FldName = 'BEG05'
     C                   CALLP     RtvRcd('BEG':wk_SenderID:wk_PurOrd#)
     C                   EVAL      wk_Data = %editc(BEG05:'X')
     C                   ENDSL

      *
      * Return the pointer to the data.
      *
     C                   EVAL      wk_DataPtr = %addr(wk_Data)
     C                   RETURN    wk_DataPtr

     P EDI850Nbr       E

     P/EJECT
      **************************************************************************
      * Retrieve EDI 850 record.                                               *
      **************************************************************************
     P RtvRcd          B

     D                 PI
     D SegmentID                      3A   CONST
     D SenderID                      15A   CONST
     D PurOrd#                       22A   CONST
     D Qualifier                     10A   CONST OPTIONS(*NOPASS:*VARSIZE)

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_SegmentID    S              3A
     D wk_SenderID     S             15A
     D wk_PurOrd#      S             22A
     D wk_Qualifier    S             10A
     D wk_BEG03        S             22A
     D wk_BEG04        S             30A

      *
      * Retrieve the numeric field data.
      *
     C                   EVAL      wk_SegmentID = SegmentID
     C                   EVAL      wk_SenderID = SenderID
     C                   EVAL      wk_PurOrd# = PurOrd#
     C                   IF        %parms > 3
     C                   EVAL      wk_Qualifier = Qualifier
     C                   ELSE
     C                   EVAL      wk_Qualifier = *Blanks
     C                   ENDIF                                                  %parms>3

     C                   IF        not %open(HMCMLF02)
     C                   OPEN      HMCMLF02
     C                   ENDIF                                                  not %open(HMCMLF02)

     C     key_          KLIST                                                  HMCMLF02
     C                   KFLD                    wk_SenderID
     C                   KFLD                    wk_PurOrd#

      // This is being done because, from Aug 2020 thru Dec 28, 2020, because
      // Dick's changed their trading partner ID, and EDR0010 (which saves 850
      // data to EDISAVE) was not changed to recognize the TP change, the
      // actual (child) PO shipped is populated in BEG04 instead of BEG03.
      /Free
       wk_BEG03 = *Blanks;
       wk_BEG04 = *Blanks;
       EXEC SQL select BEG03, BEG04
                 into :wk_BEG03, :wk_BEG04
                from EDISAVE/HMCEIPBEG
                where ISA06=:wk_SenderID and BEG04=:wk_PurOrd#
                fetch first row only;
       IF wk_BEG04 = wk_PurOrd#;
         wk_PurOrd# = wk_BEG03;
       ELSE;
         wk_BEG04 = *Blanks;
       ENDIF;                                                                // wk_BEG04=wk_PurOrd#
      /End-Free

     C                   SELECT

     C                   WHEN      wk_SegmentID = 'BEG'
     C     key_          SETGT     EIPBEGR
     C     key_          READPE    EIPBEGR
     C                   IF        %eof(HMCMLF02)
     C                   IF        not %open(EDLTPXR1)
     C                   OPEN      EDLTPXR1
     C                   ENDIF                                                  not %open(EDLTPXR1)
     C     wk_SenderID   CHAIN     EDLTPXR1
     C                   IF        %found(EDLTPXR1)
     C                   EVAL      wk_SenderID = PXFID
     C     key_          SETGT     EIPBEGR
     C                   IF        wk_BEG04 = *Blanks
     C     key_          READPE    EIPBEGR
     C                   ELSE
     C                   DOU       %eof(HMCMLF02)  or  BEG04 = wk_BEG04
     C     key_          READPE    EIPBEGR
     C                   ENDDO                                                  %eof(HMCMLF02) or
     C                   ENDIF                                                  wk_BEG04=*Blanks
     C                   ENDIF                                                  %found(EDPTXPR)
     C                   ENDIF                                                  %eof(HMCMLF02)
     C                   IF        %eof(HMCMLF02)
     C                   CLEAR     *ALL          EIPBEGR
     C                   ENDIF                                                  %eof(HMCMLF02)

     C                   WHEN      wk_SegmentID = 'N1'
     C     key_          SETGT     EIPN1R
     C                   DOU       %eof(HMCMLF02)
     C     key_          READPE    EIPN1R
     C                   IF        %eof(HMCMLF02)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HMCMLF02)
     C                   IF        N101 = wk_Qualifier
     C                   LEAVE
     C                   ENDIF                                                  N101=wk_Qualifier
     C                   ENDDO                                                  %eof(HMCMLF02)
     C                   IF        %eof(HMCMLF02)
     C                   IF        not %open(EDLTPXR1)
     C                   OPEN      EDLTPXR1
     C                   ENDIF                                                  not %open(EDLTPXR1)
     C     wk_SenderID   CHAIN     EDLTPXR1
     C                   IF        %found(EDLTPXR1)
     C                   EVAL      wk_SenderID = PXFID
     C     key_          SETGT     EIPN1R
     C                   DOU       %eof(HMCMLF02)
     C     key_          READPE    EIPN1R
     C                   IF        %eof(HMCMLF02)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HMCMLF02)
     C                   IF        N101 = wk_Qualifier
     C                   LEAVE
     C                   ENDIF                                                  N101=wk_Qualifier
     C                   ENDDO                                                  %eof(HMCMLF02)
     C                   ENDIF                                                  %found(EDPTXPR)
     C                   ENDIF                                                  %eof(HMCMLF02)
     C                   IF        %eof(HMCMLF02)
     C                   CLEAR     *ALL          EIPN1R
     C                   ENDIF                                                  %eof(HMCMLF02)

     C                   WHEN      wk_SegmentID = 'REF'
     C     key_          SETGT     EIPREFR
     C                   DOU       %eof(HMCMLF02)
     C     key_          READPE    EIPREFR
     C                   IF        %eof(HMCMLF02)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HMCMLF02)
     C                   IF        REF01 = wk_Qualifier
     C                   LEAVE
     C                   ENDIF                                                  REF01=wk_Qualifier
     C                   ENDDO                                                  %eof(HMCMLF02)
     C                   IF        %eof(HMCMLF02)
     C                   IF        not %open(EDLTPXR1)
     C                   OPEN      EDLTPXR1
     C                   ENDIF                                                  not %open(EDLTPXR1)
     C     wk_SenderID   CHAIN     EDLTPXR1
     C                   IF        %found(EDLTPXR1)
     C                   EVAL      wk_SenderID = PXFID
     C     key_          SETGT     EIPREFR
     C                   DOU       %eof(HMCMLF02)
     C     key_          READPE    EIPREFR
     C                   IF        %eof(HMCMLF02)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HMCMLF02)
     C                   IF        REF01 = wk_Qualifier
     C                   LEAVE
     C                   ENDIF                                                  REF01=wk_Qualifier
     C                   ENDDO                                                  %eof(HMCMLF02)
     C                   ENDIF                                                  %found(EDPTXPR)
     C                   ENDIF                                                  %eof(HMCMLF02)
     C                   IF        %eof(HMCMLF02)
     C                   CLEAR     *ALL          EIPREFR
     C                   ENDIF                                                  %eof(HMCMLF02)

     C                   ENDSL

     C                   RETURN

     P RtvRcd          E

     P/EJECT
      **************************************************************************
      * Retrieve parent purchase order number from child purchase order number.*
      **************************************************************************
     P RtvParentPO     B                   EXPORT

     D                 PI            30A
     D SenderID                      15A   CONST
     D ChildPO#                      22A   CONST

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_SenderID     S                   LIKE(ISA06)
     D wk_ChildPO#     S                   LIKE(BEG03)
     D wk_ParentPO#    S                   LIKE(BEG04)

     C                   EVAL      wk_SenderID = SenderID
     C                   EVAL      wk_ChildPO# = ChildPO#
     C                   EVAL      wk_ParentPO# = *Blanks

     C                   IF        not %open(HMCEIPBEG2)
     C                   OPEN      HMCEIPBEG2
     C                   ENDIF                                                  not %open(HMCEIPBEG2

     C     key_BEG       KLIST                                                  HMCEIPBEG2
     C                   KFLD                    wk_SenderID
     C                   KFLD                    wk_ChildPO#

     C     key_BEG       SETLL     HMCEIPBEG2
     C     key_BEG       READE     HMCEIPBEG2
     C                   IF        not %eof(HMCEIPBEG2)
     C                   EVAL      wk_ParentPO# = BEG04
     C     key_BEG       READE     HMCEIPBEG2
     C                   IF        not %eof(HMCEIPBEG2)
     C                   EVAL      wk_ParentPO# = *Blanks
     C                   ENDIF                                                  not %eof(HMCEIPBEG2)
     C                   ENDIF                                                  not %eof(HMCEIPBEG2)

     C                   RETURN    wk_ParentPO#

     P RtvParentPO     E

     P/EJECT
      **************************************************************************
      * Shut down service program.                                             *
      **************************************************************************
     P sd_EDR9900      B                   EXPORT
     D                 PI            10I 0

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Error        S             10I 0

     C                   EVAL      wk_Error = *Zero

     C                   DO

     C                   IF        %open(HMCMLF02)
     C                   CLOSE     HMCMLF02
     C                   ENDIF                                                  %open(HMCMLF02)

     C                   IF        %open(HMCEIPBEG2)
     C                   CLOSE     HMCEIPBEG2
     C                   ENDIF                                                  %open(HMCEIPBEG2)

     C                   IF        %open(EDLTPXR1)
     C                   CLOSE     EDLTPXR1
     C                   ENDIF                                                  %open(EDLTPXR1)

     C                   ENDDO

      *
      * Return the error code.
      *
     C                   RETURN    wk_Error

     P sd_EDR9900      E

     P/EJECT
