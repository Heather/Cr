Cr
==

 - Run Cr.exe
 - That's all, chromium is updated.

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
 
