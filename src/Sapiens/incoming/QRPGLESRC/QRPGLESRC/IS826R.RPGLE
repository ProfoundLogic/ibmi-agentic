     *=====================================================================
     *  (C) Copyright Progressive Data Solutions, Inc., 2000
     *=====================================================================
     H/TITLE IS826R - Increment Document sequence or batch sequence
     *=====================================================================
     *
     *
     *=====================================================================
     *  REVISIONS:
     *
     *=====================================================================
     * Document Index Control File
     Fimdil     uf   e           k disk

     *=====================================================================
     * Includes
     *=====================================================================
      /copy IS826RINC

     *=====================================================================
     * Stand Alone Field Definitions
     *=====================================================================
     d @WrkStnId       s              2a
     d @DocSeq         s              1a
     d @BatchSeq       s              1a
     d @RtnCode        s              1a

     D psds          esds                  extname(szq1p)

     *=====================================================================
     * PList Definitions
     *=====================================================================
     ** ENTRY PARAMETERS
     C     *entry        plist
     C                   parm                    @WrkStnId
     C                   parm                    @DocSeq
     C                   parm                    @BatchSeq
     C                   parm                    @RtnCode


     *=====================================================================
     * Main
     *=====================================================================
     c                   callp     $IncDocIdx(@WrkStnId:@DocSeq:@BatchSeq:
     c                                        @RtnCode)

     c                   eval      *inlr = *on
     *=====================================================================




     *=====================================================================
     * $IncDocIdx - Increment a document index sequence
     *=====================================================================
     p $IncDocIdx      B                   export

     *** Procedure Interface ***
     d $IncDocIdx      PI
     d  WrkStnId                      2a   const
     d  DocSeq                        1a   const
     d  BatchSeq                      1a   const
     d  RtnCode                       1a

     *** Locals ***
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)


     d changed         s              1a
     d xDate           s               d
     d xTime           s               t

     *** KLists ***
     * Document Index Control file
     C     diky1         klist
     C                   kfld                    WrkStnId

     *** Procedure Definition ***
     C                   time                    dsdttm
     c                   eval      changed = FALSE

     C     diky1         chain     pdi01                              60
     C                   if        NOT *in60

     c                   select

     c                   when      DocSeq = *on
     c                   eval      diseq# = diseq# + 1
     c                   eval      changed = TRUE

     c                   when      BatchSeq = *on
     c*** begin add *** 02/09/05 *******************************************
     c                   if        dibchs = 9999
     c                   eval      dibchs = 0
     c                   endif
     c*** end   add *** 02/09/05 *******************************************
     c                   eval      dibchs = dibchs + 1
     c                   eval      changed = TRUE

     c                   endsl

     c                   if        changed = TRUE
     C                   movel     q1user        dilusr
     C                   movel     q1pgm         dilpgm
     c                   callp     GetCurDate(xDate:xTime)
     C     *ymd          move      xDate         dildt
     C     *hms          move      xTime         diltim
    
     C                   update    pdi01

     c                   endif

     C                   else
     C                   eval      RtnCode = *on
     C                   endif

     c                   return

     p $IncDocIdx      E
