{-# OPTIONS_GHC -fno-warn-orphans #-}

module Routes where

import Import.NoFoundation

import AppType

mkYesodData "App" [parseRoutes|
/static StaticR Static appStatic

/           HomeR    GET

-- Auth
/login      LoginR   GET POST
/signup     SignupR  GET POST
/signout    SignoutR GET
/contact    ContactR GET
/forgot     ForgotR  GET POST
/reset      ResetR   GET POST

-- Admin
/organizer/signup                                        OrganizerSignupR GET POST
/conferences                                             ConferencesR GET
/conference/#ConferenceId                                ConferenceDashboardR GET
/conference/#ConferenceId/cfp/open                       ConferenceCfpOpenR POST
/conference/#ConferenceId/cfp/close                      ConferenceCfpCloseR POST
/conference/#ConferenceId/abstract-types                 ConferenceAbstractTypesR GET POST
/conference/#ConferenceId/cfp                            ConferenceCallForProposalsR GET
/conference/#ConferenceId/cfp/blocklisted                ConferenceBlockedProposalsR GET
/conference/#ConferenceId/abstract/#AbstractId           ConferenceAbstractR GET POST
/conference/#ConferenceId/abstract/#AbstractId/block     ConferenceBlockAbstractR POST
/conference/#ConferenceId/abstract/#AbstractId/unblock   ConferenceUnblockAbstractR POST

-- CFP submission
/conference/#ConferenceId/cfp/submit                     SubmitAbstractR GET POST
/conference/#ConferenceId/cfp/submitted                  SubmittedAbstractR GET
|]
