     h**********************************************************************
     h* Revisions:
     h*
     h**********************************************************************
     d* Type of Check Process
     d*
     d* Payment Updates Popup Window
     d PayDetailPrc    pr
     d payRRN                         9s 0 Const
     d*
     d* Payment Updates Popup Window
     d ValPayUpPrc     pr
     d inPayUpd                       1    Const
     d payRRN                         9s 0 Const
     d outERR                         1
     d*
     d* Voids, Stops and Reversals Reason Window
     d ValVSZRsnPrc    pr
     d inPayUpd                       1    Const
     d inPayTyp                       2    Const
     d payRRN                         9s 0 Const
     d*
     d* Voids, Stops and Reversals Reason Processing
     d ProcessVSZPrc   pr
     d inPayUpd                       1    Const
     d inPayTyp                       2    Const
     d payRRN                         9s 0 Const
     d*
     d* Check User Authority
     d CheckUserAut    pr
     d inCO#                          3p 0 Const
     d inFND                          3p 0 Const
     d*
     d* Late Payment Window
     d LatePaymentPrc  pr
     d ClmRrn                         9s 0 Const
     d inPdOnTm                       2    Const
