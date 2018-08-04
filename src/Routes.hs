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
/organizer/signup                                  OrganizerSignupR GET POST
/conferences                                       ConferencesR GET
/conference/#ConferenceId                          ConferenceDashboardR GET
/conference/#ConferenceId/abstract-types           ConferenceAbstractTypesR GET POST
/conference/#ConferenceId/cfp                      ConferenceCallForProposalsR GET
/conference/#ConferenceId/cfp/submit               SubmitAbstractR GET POST
/conference/#ConferenceId/cfp/submitted            SubmittedAbstractR GET
/conference/#ConferenceId/abstract/#AbstractId     ConferenceAbstractR GET
|]
