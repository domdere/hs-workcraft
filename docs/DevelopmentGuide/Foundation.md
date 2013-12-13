# Foundation

This module defines a lot of the characteristic behaviour of the web application.

It defines the `App` data type and its behaviour by defining its instances in the following Type Classes:

-   `Yesod`: This is the core Type Class that defines the core behaviour of all possible web apps, more detailed definition [**here**] [yesod-doc]
-   `YesodPersist`: Not all apps need a Persistence layer, so this is a seperate Type Class.  There's *some* documentation for it [**here**] [yesod-monads].
-   `YesodPersistRunner`: The [**hackage documentation**] [hackage-yesod-persist-runner] is the best online doco I can find for this.
    Provides functionality to chain DB actions using the same connection pool, an action is provided to free the connections when done.
-   `YesodAuth`: Most, but not all, apps will want user authentication and authorisation.  This lets you customise whether you want auth at all or which backend you want to use.  Hackage documentation for this typeclass is [**here**] [hackage-yesod-auth], and more exposition on **Yesod's** Authentication and Authorisation mechanisms can be found [**here**] [yesod-auth].  Supported options are:
    -   [**BrowserID**] [mozilla-browser-id]
    -   **Email**: Accounts are attached to user's emails, a verification email gets sent to their address with a link for them to click on and then they can set a password.
    -   **GoogleEmail**: Use an email address as an identifier via Google's OpenID login system
    -   **HashDB**: Looks users up in Persist where their user id and a salted SHA1 hash of their password is stored.
    -   [**OpenID**] [openid-wiki]
-   `RenderMessage`: This type class is required to use forms, allows for customised and/or internationalised form validation messages.  More detailed documentation [**here**] [yesod-internationalisation].

[yesod-doc]: http://www.yesodweb.com/book/yesod-typeclass "Yesod Type Class"
[yesod-monads]: http://www.yesodweb.com/book/yesods-monads "Yesod's Monads"
[yesod-auth]: http://www.yesodweb.com/book/authentication-and-authorization "Authentication and Authorisation with Yesod"
[yesod-internationalisation]: http://www.yesodweb.com/book/internationalization "Internationalisation with Yesod"
[hackage-yesod-persist-runner]: http://hackage.haskell.org/package/yesod-persistent-1.2.1/docs/Yesod-Persist-Core.html#t:YesodPersistRunner "Yesod.Core YesodPersistRunner"
[hackage-yesod-auth]: http://hackage.haskell.org/package/yesod-auth-1.2.0.1/docs/Yesod-Auth.html "Yesod.Auth YesodAuth"
[mozilla-browser-id]: http://en.wikipedia.org/wiki/Mozilla_Persona "Mozilla Persona - Wikipedia"
[openid-wiki]: http://en.wikipedia.org/wiki/OpenID "OpenID - Wikipedia"
