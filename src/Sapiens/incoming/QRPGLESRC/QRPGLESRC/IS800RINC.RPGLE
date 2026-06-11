     *------------------------------------------------------------------------
     * $ExSysCmd - Executes a system command
     *------------------------------------------------------------------------
     * Returns:    none
     *
     * Parameters: 200a  Command to run
     *              15p5 Length of command
     *------------------------------------------------------------------------
     d $ExSysCmd       PR                  extpgm('QCMDEXC')
     d  cmd                         200a   Options(*varsize) const
     d  cmdlen                       15p 5 const


     *------------------------------------------------------------------------
     * $GetDloEnt - Loads a member in SAFLP with a directories entries
     *------------------------------------------------------------------------
     * Returns:    none
     *
     * Parameters:  80a  Directory to scan
     *               1a  Error code
     *------------------------------------------------------------------------
     d $GetDloEnt      PR                  extpgm('IS810C')
     d  folder                       80a
     d  doctype                       8a
     d  wrkstnid                      2a
     d  errCode                       1a
     d  dataq                        10a
     d  dataqlib                     10a




     *------------------------------------------------------------------------
     * ErrHandler - Handles any error messages.
     *------------------------------------------------------------------------
     * Returns:    none
     *
     * Parameters:   1a  Return code from program that caused error
     *              10a  Program that caused error
     *------------------------------------------------------------------------
     D ErrHandler      PR
     D  errCode                       1a   const                                Return code
     D  errPgm                       10a   const                                Program




     D $CheckQuit      PR                  extpgm('IS803C')
     D  dataq                        10a
     D  dataqlib                     10a
     D  rtnCode                       1a

     *------------------------------------------------------------------------
     * $CPYDloEnt - Loads a member in SAFLP with a directories entries
     *------------------------------------------------------------------------
     * Returns:    none
     *
     * Parameters:  80a  Directory to scan
     *               1a  Error code
     *------------------------------------------------------------------------
     d $CpyDloEnt      PR                  extpgm('IS811C')
     d  folder                       80a
     d  doctype                       8a
     d  wrkstnid                      2a
     d  errCode                       1a
     d  dataq                        10a
     d  dataqlib                     10a



