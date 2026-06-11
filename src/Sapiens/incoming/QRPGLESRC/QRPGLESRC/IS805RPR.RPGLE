     *------------------------------------------------------------------------
     * GetErrCode   Get an error code
     *------------------------------------------------------------------------
     d GetErrCode      PR            80a
     D  eErrCode                      8a   const
     D  eCategory                    10a   options(*NOPASS)
     D  eSubCat                      10a   options(*NOPASS)
