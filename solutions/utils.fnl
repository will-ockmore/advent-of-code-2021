(local curl (require :cURL))

(fn request [url]
  (let [out []]
    (with-open [h (curl.easy {: url
                              :httpheader []
                              :writefunction {:write #(table.insert out $2)}})]
      (h:perform)
      out)))

(fn get-contents [filepath]
  (icollect [line _ (with-open [f (io.open filepath)]
                      (string.gmatch (f:read :*a) "(.-)\n"))]
    line))

(fn any [xs]
  (accumulate [result false _ v (ipairs xs)]
    (or result v)))

(fn all [xs]
  (accumulate [result true _ v (ipairs xs)]
    (and result v)))


{: request : get-contents : any : all}
