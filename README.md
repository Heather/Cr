Cr
==

[![Build Status](https://travis-ci.org/Heather/Cr.png?branch=master)](https://travis-ci.org/Heather/Cr)

Smart chromium installer / updater

Binary
------

 - Run Cr
 - That's all, chromium is updated to last version.

```haskell
getChromium :: [Char] → [Char] → IO()
getChromium s v = withSocketsDo $ do
    let url = "http://commondatastorage.googleapis.com/chromium-browser-snapshots/" 
                        ++ s ++ "/" ++ v ++ "/mini_installer.exe"
    irequest <- liftIO $ parseUrl url
    withManager $ \manager → do
        let request = irequest
             { method = methodGet }
        response <- http request manager
        responseBody response C.$$+- sinkFile "mini-installer.exe"
```

 - Run Cr --platform="Win" to get chromium for specified platform (last by default)
 - Run Cr --build to get specified build version
 - Run Cr --last to see what is last version
