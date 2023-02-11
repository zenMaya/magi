(define-module (magi system web)
  #:use-module (guix gexp)
  #:use-module (gnu services web)
  #:use-module (gnu services certbot))

(define %nginx-deploy-hook
  (program-file
   "nginx-deploy-hook"
   #~(let ((pid (call-with-input-file "/var/run/nginx/pid" read)))
       (kill pid SIGHUP))))

(define-public http-redirect-to-https
  (nginx-server-configuration
   (listen '("80"))
   (server-name (list "_"))
   (raw-content '("return 301 https://$host$request_uri;"))))

(define (reverse-proxy port)
  (nginx-location-configuration
                (uri "/")
                (body `(,(string-append "proxy_pass http://127.0.0.1:" port ";")
                        "proxy_set_header Host $host;"
                        "proxy_set_header X-Real-IP $remote_addr;"
                        "proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;"
                        "proxy_set_header X-Forwarded-Host $server_name;"
                        "proxy_read_timeout 1200s;"))))

(define-public transmission-certbot-configuration
  (certificate-configuration
   (domains '("transmission.omase.tk"))
   (deploy-hook %nginx-deploy-hook)))

(define-public transmission-nginx-configuration
  (nginx-server-configuration
   (locations (list
               (reverse-proxy "9091")))
   (server-name '("transmission.omase.tk" "transmission.melchior.local"))
   (listen '("443 ssl http2" "[::]:443 ssl http2"))
   (ssl-certificate "/etc/letsencrypt/live/transmission.omase.tk/fullchain.pem")
   (ssl-certificate-key "/etc/letsencrypt/live/transmission.omase.tk/privkey.pem")))

(define* (jellyfin-proxy-pass location proxy-pass #:rest other-options)
  (nginx-location-configuration
   (uri location)
   (body `(,(string-append "proxy_pass " proxy-pass ";")
           "proxy_set_header Host $host;"
           "proxy_set_header X-Real-IP $remote_addr;"
           "proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;"
           "proxy_set_header X-Forwarded-Proto $scheme;"
           "proxy_set_header X-Forwarded-Protocol $scheme;"
           "proxy_set_header X-Forwarded-Host $http_host;"
           ,@other-options))))

(define-public jellyfin-nginx-configuration
  (nginx-server-configuration
   (server-name '("media.omase.tk" "media.melchior.local"))
   (listen '("443 ssl http2" "[::]:443 ssl http2"))
   (locations (list
               (nginx-location-configuration
                (uri "= /")
                (body '("return 302 https://$host/web/;")))
               (jellyfin-proxy-pass "/" "http://$jellyfin:8096" "proxy_buffering off;")
               (jellyfin-proxy-pass "= /web/" "http://$jellyfin:8096/web/index.html")
               (jellyfin-proxy-pass "/socket" "http://$jellyfin:8096"
                                    "proxy_http_version 1.1;"
                                    "proxy_set_header Upgrade $http_upgrade;"
                                    "proxy_set_header Connection \"upgrade\";")))
   (ssl-certificate "/etc/letsencrypt/live/transmission.omase.tk/fullchain.pem")
   (ssl-certificate-key "/etc/letsencrypt/live/transmission.omase.tk/privkey.pem")
   (raw-content '("client_max_body_size 20M;"
                  "set $jellyfin 127.0.0.1;"
                  "add_header X-Frame-Options \"SAMEORIGIN\";"
                  "add_header X-XSS-Protection \"1; mode=block\";"
                  "add_header X-Content-Type-Options \"nosniff\";"
                  "add_header Content-Security-Policy \"default-src https: data: blob: http://image.tmdb.org; style-src 'self' 'unsafe-inline'; script-src 'self' 'unsafe-inline' https://www.gstatic.com/cv/js/sender/v1/cast_sender.js https://www.gstatic.com/eureka/clank/95/cast_sender.js https://www.gstatic.com/eureka/clank/96/cast_sender.js https://www.gstatic.com/eureka/clank/97/cast_sender.js https://www.youtube.com blob:; worker-src 'self' blob:; connect-src 'self'; object-src 'none'; frame-ancestors 'self'\";"))))
