      *=====================================================================
      * Prototypes for User Profiles and User Activity
     *========================================================================
      *
     *========================================================================
      * Retrieve the profiles associated with the user
      *=====================================================================
     dGetAssociatedProfiles...
     d                 pr            10    dim(20)
     d inUserProfile                 10    const options(*nopass)
      //----------------------------------------------------------------------
      // PriorSelection - Saves/Retrieves users Prior Screen Selections
      //----------------------------------------------------------------------
     D PriorSelection  pr             1n
     d inCompany                      3s 0 const
     d inUser                        10a   const
     d inScreenFile                  10a   const
     d inMode                         1a   const
     d Data                        6500a   varying
