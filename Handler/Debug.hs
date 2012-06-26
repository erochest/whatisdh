
module Handler.Debug
    ( getDebugUsersR
    ) where

import Import

getDebugUsersR :: Handler RepJson
getDebugUsersR = (runDB $ selectList [] [Asc UserIdent]) >>= jsonToRepJson 

