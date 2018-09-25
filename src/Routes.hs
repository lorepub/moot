{-# OPTIONS_GHC -fno-warn-orphans #-}

module Routes where

import Import.NoFoundation

import AppType

mkYesodData "App" [parseRoutes|
/static       StaticR Static appStatic

/             HomeR    GET

-- Auth
/login        LoginR   GET POST
/signup       SignupR  GET POST
/verify/#UUID VerifyR  GET
/signout      SignoutR GET
/about        AboutR   GET
/contact      ContactR GET
/forgot       ForgotR  GET POST
/reset        ResetR   GET POST

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
/conference/#ConferenceId/surrogate/abstract             ConferenceSurrogateAbstractR GET POST
/user/search/#Text                                       UserSearchR GET

-- CFP submission
/conference/#ConferenceId/cfp/submit                     SubmitAbstractR GET POST
/conference/#ConferenceId/cfp/draft/submit               SubmitAbstractDraftR POST
/conference/#ConferenceId/cfp/#AbstractId/draft          AbstractDraftR GET POST
/conference/#ConferenceId/cfp/submitted                  SubmittedAbstractR GET

-- Proof of concept slug based URLs
/conference2/#ConferenceSlug/cfp/submitted               SubmittedAbstractPocR GET
/conference2/#ConferenceSlug/abstract/#AbstractId        ConferenceAbstractPocR GET POST

|]
