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

-- Admin
/organizer/signup                 OrganizerSignupR GET POST
/conferences                      ConferencesR GET
/conference/#Int64                ConferenceDashboardR GET
/conference/#Int64/abstract-types ConferenceAbstractTypesR GET POST
/conference/#Int64/cfp            ConferenceCallForProposalsR GET

/cfp/submit SubmitAbstractR GET POST
|]
