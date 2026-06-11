     *------------------------------------------------------------------------
     * $GetDloEnt - Retrieves the dlo entries in a folder
     *------------------------------------------------------------------------
     * Parameters:  80a    (input)  Folder
     *               1a    (i/o)    Error code
     *------------------------------------------------------------------------
     d $GetDloEnt      PR                  extpgm('IS820C')
     d   folder                      80a
     d   errCode                      1a




     *------------------------------------------------------------------------
     * $CpyDlo - Copies a dlo different folder
     *------------------------------------------------------------------------
     * Parameters:  12a    (input)  From doc
     *              80a    (input)  From folder
     *              12a    (input)  To doc
     *              80a    (input)  To folder
     *               1a    (output) Return code
     *------------------------------------------------------------------------
     d $CpyDlo         PR                  extpgm('IS801C')
     d   fromDoc                     12a
     d   fromFolder                  80a
     d   toDoc                       12a
     d   toFolder                    80a
     d   rtnCode                      1a




     *------------------------------------------------------------------------
     * $DltDlo - Deletes a dlo
     *------------------------------------------------------------------------
     * Parameters:  12a    (input)  Document to delete
     *              80a    (input)  Folder
     *               1a    (output) Return code
     *------------------------------------------------------------------------
     d $DltDlo         PR                  extpgm('IS802C')
     d   doc                         12a
     d   folder                      80a
     d   rtnCode                      1a




     *---------------------------------------------------------------------
     * $IncDocIdx - Increment a document index sequence
     *---------------------------------------------------------------------
     d $IncDocIdx      PR
     d  WrkStnId                      2a   const
     d  DocSeq                        1a   const
     d  BatchSeq                      1a   const
     d  RtnCode                       1a

