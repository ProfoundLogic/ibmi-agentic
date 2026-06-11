     *---------------------------------------------------------------------
     * $IncDocIdx - Increment a document index sequence
     *---------------------------------------------------------------------
     d $IncDocIdx      PR
     d  WrkStnId                      2a   const
     d  DocSeq                        1a   const
     d  BatchSeq                      1a   const
     d  RtnCode                       1a

      *=====================================================================
      * Cvt2cymd
      *=====================================================================
      * Description: Converts a numeric date in *MDYY format to *CYMD format
      *
      * Parameter 1:  8.0 - date to convert
      *
      * Returns:      7.0 - converted date (zero if invalid)
      *=====================================================================
     D Cvt2cymd        PR             7  0
     D  date8                         8  0 Const

     d GetCurDate      PR
     d  dDate                          d
     d  tTime                          t   options(*nopass)



     *---------------------------------------------------------------------
     * Stand-Alone Field Definitions
     *---------------------------------------------------------------------
     d TRUE            C                   CONST('1')
     d FALSE           C                   CONST('0')
