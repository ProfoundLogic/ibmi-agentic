     h*  (C) Copyright Progressive Data Solutions, Inc., 2005
     h**********************************************************************************************
     h/TITLE FIXSMARP - FIX SMARP
     h option(*nodebugio:*srcstmt)
     h**********************************************************************************************
     h* Revisions:                                                                                 *
     h*                                                                                            *
     h**********************************************************************************************
     fsmarp     uf   e             disk
     fqsysprt   o    f  210        printer oflind(*inoa)                        error report
     d*=============================================================================================
     d* Prototypes
     d*=============================================================================================
     d fixSmarpSeq#    pr
     d getSeq#         pr             5s 0
     d getAddSeq#      pr             5s 0
     d printRecord     pr
     d programStart    pr
     d arSequence      s              5s 0 inz(0)
     d sys_date        s               d   datfmt(*usa)
     d sys_time        s               t   timfmt(*hms:)
     d psds          esds                  extname(szq1p)
     c*
     c* Initialization
     c*
     c                   read      par01
     c                   dow       not %eof
     c                   callp     fixSmarpSeq#
     c                   read      par01
     c                   enddo

     c                   eval      *inlr = *on
     oqsysprt   e            head           1 06
     o                       sys_time
     o                                          135 'AR transfer'
     o                       sys_date           201
     o          e            head           2
     o                       q1pgm
     o                                          195 'Page:'
     o                       page          z    201
     o          e            head           0
     o                                           20 'Key                 '
     o                                           40 '                    '
     o                                         +002 ' Seq#'
     o                                         +002 'Aseq#'
     o          e            det            1
     o                       arkey               40
     o                       arseq#        4   +002
     o                       arasq#        4   +002
     c*------------------------------------------------------------------------*
     c* fixSmarpSeq# - fix the sequence number
     c*------------------------------------------------------------------------*
     p fixSmarpSeq#    B
     d fixSmarpSeq#    PI

     c                   if        arseq# = 0
     c                   eval      arSequence = getSeq#
     c                   if        arSequence <> 0
     c                   eval      arseq# = arSequence
     c                   eval      arasq# = getAddSeq#
     c                   callp     printRecord
     c                   endif
     c                   endif
     c                   update    par01

     p fixSmarpSeq#    E
     c*------------------------------------------------------------------------*
     c* getAddSeq#  - gets the next available additional seq#
     c*------------------------------------------------------------------------*
     p getAddSeq#      B
     d getAddSeq#      PI             5s 0

     d sequence        s              5s 0 inz(1)

     c*
     c/EXEC SQL
     c+ DECLARE PROJECT2 CURSOR FOR
     c+ SELECT MAX(ARASQ#)
     c+ FROM SMARP
     c+ WHERE ARKEY = :ARKEY AND ARSEQ# = :ARSEQ#
     c/END-EXEC
     c*
     c/EXEC SQL
     c+ OPEN PROJECT2
     c/END-EXEC
     c*
     c                   if        sqlstt = '00000'
     c*
     c/EXEC SQL
     c+ FETCH NEXT
     c+ FROM PROJECT2
     c+ INTO  :SEQUENCE
     c/END-EXEC
     c*
     c                   endif

     c                   return    sequence

     p getAddSeq#      E
     c*------------------------------------------------------------------------*
     c* getSeq#  - gets the remarks sequence #
     c*------------------------------------------------------------------------*
     p getSeq#         B
     d getSeq#         PI             5s 0

     d sequence        s              5s 0 inz(0)

     c*
     c/EXEC SQL
     c+ DECLARE PROJECT1 CURSOR FOR
     c+ SELECT CMSEQ#
     c+ FROM SDCMP
     c+ WHERE CMKEY = :ARKEY AND CMTRDT = :ARTRDT AND
     c+       CMIUSR = :ARIUSR AND (CMTRTM = :ARTRTM OR
     C+       CMTRTM = :ARTRTM - 1) AND CMRMCD = :ARRMCD
     c/END-EXEC
     c*
     c/EXEC SQL
     c+ OPEN PROJECT1
     c/END-EXEC
     c*
     c                   if        sqlstt = '00000'
     c*
     c/EXEC SQL
     c+ FETCH NEXT
     c+ FROM PROJECT1
     c+ INTO  :SEQUENCE
     c/END-EXEC
     c*
     c                   endif

     c                   return    sequence

     p getSeq#         E
     c*------------------------------------------------------------------------*
     c* printRecord - print record that was changed
     c*------------------------------------------------------------------------*
     p printRecord     B
     d printRecord     PI

     c                   if        *inoa
     c                   except    head
     c                   endif
     c                   except    det

     p printRecord     E
     c*------------------------------------------------------------------------*
     c* programStart - Initialization
     c*------------------------------------------------------------------------*
     p programStart    B
     d programStart    PI

     c*
     c* Get report info.
     c*
     c     *hms          move      q1time        sys_time
     c     *usa          move      *date         sys_date
     c                   except    head


     p programStart    E
