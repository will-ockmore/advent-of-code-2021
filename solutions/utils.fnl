(local curl (require :cURL))

(fn request [url]
  (let [out []]
    (with-open [h (curl.easy {:url url
                              :httpheader []
                              :writefunction {:write #(table.insert out $2)}})]
               (h:perform)
               out)))

(fn get-contents [filepath] 
  (icollect [line _ (with-open [f (io.open filepath)]
                        (string.gmatch (f:read :*a) "(.-)\n"))] line))
{: request
 : get-contents}
