(local curl (require :cURL))

(fn request [url]
  (let [out []]
    (with-open [h (curl.easy {:url url
                              :httpheader []
                              :writefunction {:write #(table.insert out $2)}})]
               (h:perform)
               out)))

(fn get-contents [filepath] 
  (with-open [f (io.open filepath)]
             (f:read :*a)))
{: request
 : get-contents}
