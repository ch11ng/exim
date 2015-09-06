;;; exim.el --- Emacs X Input Method  -*- lexical-binding: t -*-

;; Copyright (C) 2015 Chris Feng

;; Author: Chris Feng <chris.w.feng@gmail.com>
;; Maintainer: Chris Feng <chris.w.feng@gmail.com>
;; Version: 0
;; Package-Requires: ((xelb "0.1"))
;; Keywords: unix, i18n
;; URL: https://github.com/ch11ng/exim

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; EXIM (Emacs X Input Method) is an X input method server utilizing Emacs as
;; its backend. It's mainly designed for working with EXWM window manger.
;; Considering that Emacs ships with a bunch of builtin input methods (in LEIM)
;; and it's easy to make new ones for it, EXIM can support a large variety of
;; input methods.

;; Note that this is not the only input method working in EXWM; perhaps other
;; IM servers also work. This just provides an alternative.

;; The XIM protocol is quite flexible by itself, stating that an implementation
;; can create network connections of various types as well as make use of an
;; existing X connection for communication, and that an IM server may support
;; multiple transport versions, various input styles and several event flow
;; modals, etc. Here we only make choices that are most popular among other
;; IM servers and more importantly, practical for Emacs to act as a IM server:
;; + Packets are transported on top of an X connection, as is adopted by most
;;   IMEs.
;; + Only transport version 0.0 (i.e. only-CM & Property-with-CM) is supported
;;   (same as "IM Server Developers Kit", which is used by most IMEs).
;; + Only support static event flow, on-demand-synchronous method.
;; + Only "root-window" input style is supported.

;; To enable EXIM for EXWM, follow these steps:
;; 0. Install exwm first.
;; 1. Save EXIM to your disk and make sure it's in `load-path'.
;; 2. Add following lines to your Emacs init file:
;;
;;    (require 'exim)
;;    (add-hook 'exwm-init-hook 'exim-start)
;;
;; 3. Bind a key to toggle input method. Typically you should choose 'C-\'.
;; 4. Insert following lines to '~/.xinitrc':
;;
;;    export XMODIFIERS=@im=exim
;;    export GTK_IM_MODULE=xim
;;    export QT_IM_MODULE=xim
;;    export CLUTTER_IM_MODULE=xim
;;

;; Todo:
;; + Exploit input-method-[de]activate-hook to reduce unnecessary traffic.
;; + Are strings NULL-terminated? (UIM)
;; + Some requests should be handled synchronously.

;; References:
;; + XIM (http://www.x.org/releases/X11R7.6/doc/libX11/specs/XIM/xim.html)
;; + IMdkit (http://xorg.freedesktop.org/archive/unsupported/lib/IMdkit/)
;; + UIM (https://github.com/uim/uim)

;;; Code:

(require 'xcb-xim)
(require 'xcb-keysyms)

(eval-and-compile
  (defvar exim-debug-on nil "Non-nil to turn on debug for EXIM."))

(defmacro exim--log (format-string &rest args)
  "Print debug message."
  (when exim-debug-on
    `(message (concat "[EXIM] " ,format-string) ,@args)))

;; Locales from glibc
(defconst exim--locales
  "@locale=\
aa,af,ak,am,an,anp,ar,as,ast,ayc,az,be,bem,ber,bg,bhb,bho,bn,bo,br,brx,bs,byn,\
ca,ce,cmn,crh,cs,csb,cv,cy,da,de,doi,dv,dz,el,en,es,et,eu,fa,ff,fi,fil,fo,fr,\
fur,fy,ga,gd,gez,gl,gu,gv,ha,hak,he,hi,hne,hr,hsb,ht,hu,hy,ia,id,ig,ik,is,it,\
iu,iw,ja,ka,kk,kl,km,kn,ko,kok,ks,ku,kw,ky,lb,lg,li,li,lij,lo,lt,lv,lzh,mag,\
mai,mg,mhr,mi,mk,ml,mn,mni,mr,ms,mt,my,nan,nb,nds,ne,nhn,niu,nl,nn,nr,nso,oc,\
om,or,os,pa,pa,pap,pl,ps,pt,quz,raj,ro,ru,rw,sa,sat,sc,sd,se,shs,si,sid,sk,sl,\
so,sq,sr,ss,st,sv,sw,szl,ta,tcy,te,tg,th,the,ti,tig,tk,tl,tn,tr,ts,tt,ug,uk,\
unm,ur,uz,ve,vi,wa,wae,wal,wo,xh,yi,yo,yue,zh,zu,\
C,no")

(defvar exim--internal
  '(connection nil root nil window nil im-id 0 ic-id 0 property-index 0
               LOCALES nil TRANSPORT nil _XIM_XCONNECT nil _XIM_PROTOCOL nil
               event-pending nil)
  "Plist stores the internal status of EXIM.")

(defun exim--on-SelectionRequest (data _synthetic)
  "Handle SelectionRequest event on IMS window."
  (let ((obj (make-instance 'xcb:SelectionRequest))
        (connection (plist-get exim--internal 'connection))
        value)
    (xcb:unmarshal obj data)
    (with-slots (time requestor selection target property) obj
      (setq value (if (= target (plist-get exim--internal 'LOCALES))
                      exim--locales
                    (when (= target (plist-get exim--internal 'TRANSPORT))
                      "@transport=X/")))
      (when value
        ;; Change the property
        (xcb:+request connection
            (make-instance 'xcb:ChangeProperty
                           :mode xcb:PropMode:Replace
                           :window requestor
                           :property property
                           :type target
                           :format 8
                           :data-len (length value)
                           :data value))
        ;; Send SelectionNotify event
        (xcb:+request connection
            (make-instance 'xcb:SendEvent
                           :propagate 0
                           :destination requestor
                           :event-mask xcb:EventMask:NoEvent
                           :event (xcb:marshal
                                   (make-instance 'xcb:SelectionNotify
                                                  :time time
                                                  :requestor requestor
                                                  :selection selection
                                                  :target target
                                                  :property property)
                                   connection)))
        (xcb:flush connection)))))

(defun exim--on-ClientMessage-0 (data _synthetic)
  "Handle ClientMessage event on IMS window (new connection)."
  (let ((obj (make-instance 'xcb:ClientMessage))
        (connection (plist-get exim--internal 'connection))
        (root (plist-get exim--internal 'root))
        data32 client-window new-connection server-window)
    (xcb:unmarshal obj data)
    (cl-assert (= (slot-value obj 'type)
                  (plist-get exim--internal '_XIM_XCONNECT)))
    ;; Create new X connection
    (setq data32 (slot-value (slot-value obj 'data) 'data32)
          client-window (elt data32 0)
          new-connection (xcb:connect-to-socket)
          server-window (xcb:generate-id new-connection))
    (set-process-query-on-exit-flag (slot-value new-connection 'process) nil)
    ;; Store this client
    (plist-put exim--internal server-window
               (vector new-connection client-window nil))
    ;; Listen for ClientMessage event on the new connection
    (xcb:+event new-connection 'xcb:ClientMessage 'exim--on-ClientMessage)
    ;; Create a communication window
    (xcb:+request new-connection
        (make-instance 'xcb:CreateWindow
                       :depth 0 :wid server-window :parent root
                       :x 0 :y 0 :width 1 :height 1 :border-width 0
                       :class xcb:WindowClass:InputOutput :visual 0
                       :value-mask xcb:CW:OverrideRedirect
                       :override-redirect 1))
    (xcb:flush new-connection)
    ;; Send connection establishment ClientMessage
    (setf (slot-value obj 'window) client-window
          (slot-value (slot-value obj 'data) 'data32) (list server-window
                                                            0 0 0 0))
    (slot-makeunbound (slot-value obj 'data) 'data8)
    (slot-makeunbound (slot-value obj 'data) 'data16)
    (xcb:+request connection
        (make-instance 'xcb:SendEvent
                       :propagate 0
                       :destination client-window
                       :event-mask xcb:EventMask:NoEvent
                       :event (xcb:marshal obj connection)))
    (xcb:flush connection)))

(defun exim--on-ClientMessage (data _synthetic)
  "Handle ClientMessage event on IMS communication window (request)."
  (let ((obj (make-instance 'xcb:ClientMessage))
        server-window client-window connection)
    (xcb:unmarshal obj data)
    (setq server-window (slot-value obj 'window)
          connection (plist-get exim--internal server-window)
          client-window (elt connection 1)
          connection (elt connection 0))
    (cl-assert (= (slot-value obj 'type)
                  (plist-get exim--internal '_XIM_PROTOCOL)))
    (pcase (slot-value obj 'format)
      (8 ;; Data
       (exim--on-request (vconcat (slot-value (slot-value obj 'data) 'data8))
                         connection client-window server-window))
      (32 ;; Atom
       (setq data (slot-value (slot-value obj 'data) 'data32)
             data (slot-value
                   (xcb:+request-unchecked+reply connection
                       (make-instance 'xcb:GetProperty
                                      :delete 1
                                      :window server-window
                                      :property (elt data 1)
                                      :type xcb:GetPropertyType:Any
                                      :long-offset 0
                                      :long-length (elt data 0)))
                   'value))
       (exim--on-request data connection client-window server-window))
      (_ (cl-assert nil)))))

(defun exim--on-request (data connection client-window server-window)
  "Handle XIM reuqest."
  (let ((opcode (elt data 0))
        (xim:lsb (elt (plist-get exim--internal server-window) 2))) ;temp
    (cond ((= opcode xim:opcode:error)
           (exim--log "ERROR: %s" data))
          ((= opcode xim:opcode:connect)
           (exim--log "CONNECT")
           (setq xim:lsb (= (elt data 4) xim:connect-byte-order:lsb-first))
           ;; Store byte-order
           (setf (elt (plist-get exim--internal server-window) 2) xim:lsb)
           (let ((obj (make-instance 'xim:connect)))
             (xcb:unmarshal obj data)
             (exim--send (if (and (= 1 (slot-value obj 'major-version))
                                  (= 0 (slot-value obj 'minor-version))
                                  ;; No authentication support
                                  (= 0 (slot-value obj 'number)))
                             (make-instance 'xim:connect-reply)
                           (make-instance 'xim:error
                                          :im-id 0 :ic-id 0
                                          :flag xim:error-flag:invalid-both
                                          :error-code
                                          xim:error-code:bad-something
                                          :length 0 :type 0 :detail nil))
                         connection client-window server-window)))
          ;; No authentication support
          ((or (= opcode xim:opcode:auth-required)
               (= opcode xim:opcode:auth-reply)
               (= opcode xim:opcode:auth-next)
               (= opcode xim:opcode:auth-ng))
           (exim--send (make-instance 'xim:error
                                      :im-id 0 :ic-id 0
                                      :flag xim:error-flag:invalid-both
                                      :error-code xim:error-code:bad-something
                                      :length 0 :type 0 :detail nil)
                       connection client-window server-window))
          ((= opcode xim:opcode:disconnect)
           (exim--log "DISCONNECT")
           (cl-remf exim--internal server-window)
           (exim--send (make-instance 'xim:disconnect-reply)
                       connection client-window server-window)
           ;; Destroy the communication window
           (xcb:+request connection
               (make-instance 'xcb:DestroyWindow :window server-window))
           ;; Disconnect from the connection
           (xcb:disconnect connection))
          ((= opcode xim:opcode:open)
           (exim--log "OPEN")
           ;; No check is needed; simply make the reply
           (let ((im-id (plist-get exim--internal 'im-id))
                 (im-attrs
                  (list
                   (make-instance 'xim:XIMATTR
                                  :id 0
                                  :type xim:ATTRIBUTE-VALUE-TYPE:xim-styles
                                  :length (length xlib:XNQueryInputStyle)
                                  :attribute xlib:XNQueryInputStyle)))
                 (ic-attrs
                  (list
                   (make-instance 'xim:XICATTR
                                  :id 0
                                  :type xim:ATTRIBUTE-VALUE-TYPE:long-data
                                  :length (length xlib:XNInputStyle)
                                  :attribute xlib:XNInputStyle)
                   (make-instance 'xim:XICATTR
                                  :id 1
                                  :type xim:ATTRIBUTE-VALUE-TYPE:window
                                  :length (length xlib:XNClientWindow)
                                  :attribute xlib:XNClientWindow)
                   ;; Required by e.g. xterm
                   (make-instance 'xim:XICATTR
                                  :id 2
                                  :type xim:ATTRIBUTE-VALUE-TYPE:window
                                  :length (length xlib:XNFocusWindow)
                                  :attribute xlib:XNFocusWindow))))
             (plist-put exim--internal 'im-id (1+ im-id))
             (exim--send (make-instance 'xim:open-reply
                                        :im-id im-id
                                        :im-attrs-length nil ;auto set
                                        :im-attrs im-attrs
                                        :ic-attrs-length nil ;auto set
                                        :ic-attrs ic-attrs)
                         connection client-window server-window)
             ;; Static event flow, on-demand-synchronous method
             (exim--send
              (make-instance 'xim:set-event-mask
                             :im-id im-id
                             :ic-id 0 ;all
                             :forward-event-mask xcb:EventMask:KeyPress
                             :synchronous-event-mask xcb:EventMask:NoEvent)
              connection client-window server-window)))
          ((= opcode xim:opcode:close)
           (exim--log "CLOSE")
           (let ((obj (make-instance 'xim:close)))
             (xcb:unmarshal obj data)
             (exim--send (make-instance 'xim:close-reply
                                        :im-id (slot-value obj 'im-id))
                         connection client-window server-window)))
          ((= opcode xim:opcode:trigger-notify)
           (exim--log "TRIGGER-NOTIFY")
           ;; Only static event flow modal is supported
           (exim--send (make-instance 'xim:error
                                      :im-id 0 :ic-id 0
                                      :flag xim:error-flag:invalid-both
                                      :error-code xim:error-code:bad-something
                                      :length 0 :type 0 :detail nil)
                       connection client-window server-window))
          ((= opcode xim:opcode:encoding-negotiation)
           (exim--log "ENCODING-NEGOTIATION")
           (let ((obj (make-instance 'xim:encoding-negotiation))
                 index)
             (xcb:unmarshal obj data)
             ;; FIXME: UTF-8 encoding does not work (not decoded by client)
             (setq index (cl-position "COMPOUND_TEXT"
                                      (mapcar (lambda (i) (slot-value i 'name))
                                              (slot-value obj 'names))
                                      :test 'equal))
             (unless index
               ;; Fallback to portable character encoding (a subset of ASCII)
               (setq index -1))
             (exim--send (make-instance
                          'xim:encoding-negotiation-reply
                          :im-id (slot-value obj 'im-id)
                          :category
                          xim:encoding-negotiation-reply-category:name
                          :index index)
                         connection client-window server-window)))
          ((= opcode xim:opcode:query-extension)
           (exim--log "QUERY-EXTENSION")
           (let ((obj (make-instance 'xim:query-extension)))
             (xcb:unmarshal obj data)
             (exim--send (make-instance 'xim:query-extension-reply
                                        :im-id (slot-value obj 'im-id)
                                        ;; No extension support
                                        :length 0 :extensions nil)
                         connection client-window server-window)))
          ((= opcode xim:opcode:set-im-values)
           (exim--log "SET-IM-VALUES")
           ;; There's only one possible input method attribute; simply reply
           (let ((obj (make-instance 'xim:set-im-values)))
             (xcb:unmarshal obj data)
             (exim--send (make-instance 'xim:set-im-values-reply
                                        :im-id (slot-value obj 'im-id))
                         connection client-window server-window)))
          ((= opcode xim:opcode:get-im-values)
           (exim--log "GET-IM-VALUES")
           (let ((obj (make-instance 'xim:get-im-values))
                 (im-attributes
                  (list
                   (make-instance
                    'xim:XIMATTRIBUTE
                    :id 0
                    :length nil         ;auto set
                    :value (make-instance 'xim:XIMStyles
                                          :number nil ;auto set
                                          ;; Input sytle: root-window
                                          :styles
                                          (list
                                           (logior xlib:XIMPreeditNothing
                                                   xlib:XIMStatusNothing))))))
                 im-attributes-id)
             (xcb:unmarshal obj data)
             (setq im-attributes-id (slot-value obj 'im-attributes-id))
             (exim--send
              (if (cl-notevery (lambda (i) (< i (length im-attributes)))
                               im-attributes-id)
                  (make-instance 'xim:error
                                 :im-id (slot-value obj 'im-id)
                                 :ic-id 0
                                 :flag xim:error-flag:invalid-ic-id
                                 :error-code xim:error-code:bad-something
                                 :length 0 :type 0 :detail nil)
                (make-instance 'xim:get-im-values-reply
                               :im-id (slot-value obj 'im-id)
                               :length nil ;auto set
                               :im-attributes (mapcar
                                               (lambda (i)
                                                 (elt im-attributes i))
                                               im-attributes-id)))
              connection client-window server-window)))
          ((= opcode xim:opcode:create-ic)
           (exim--log "CREATE-IC")
           (let ((ic-id (plist-get exim--internal 'ic-id))
                 (obj (make-instance 'xim:create-ic)))
             ;; FIXME: better be locally incremental
             (plist-put exim--internal 'ic-id (1+ ic-id))
             (xcb:unmarshal obj data)
             ;; ic-attributes slot is simply ignored.
             (exim--send (make-instance 'xim:create-ic-reply
                                        :im-id (slot-value obj 'im-id)
                                        :ic-id ic-id)
                         connection client-window server-window)))
          ((= opcode xim:opcode:destroy-ic)
           (exim--log "DESTROY-IC")
           (let ((obj (make-instance 'xim:destroy-ic)))
             (xcb:unmarshal obj data)
             (exim--send (make-instance 'xim:destroy-ic-reply
                                        :im-id (slot-value obj 'im-id)
                                        :ic-id (slot-value obj 'ic-id))
                         connection client-window server-window)))
          ((= opcode xim:opcode:set-ic-values)
           (exim--log "SET-IC-VALUES")
           (let ((obj (make-instance 'xim:set-ic-values)))
             (xcb:unmarshal obj data)
             ;; We don't distinguish between input contexts; simply reply here
             (exim--send (make-instance 'xim:set-ic-values-reply
                                        :im-id (slot-value obj 'im-id)
                                        :ic-id (slot-value obj 'ic-id))
                         connection client-window server-window)))
          ((= opcode xim:opcode:get-ic-values)
           (exim--log "GET-IC-VALUES")
           (let ((obj (prog1 (make-instance 'xim:get-ic-values)
                        (xcb:unmarshal obj data)))
                 (ic-attributes
                  (list
                   (make-instance 'xim:XICATTRIBUTE
                                  :id 0
                                  :length 4
                                  :value (funcall
                                          (if xim:lsb
                                              'xcb:-pack-u4-lsb
                                            'xcb:-pack-u4)
                                          (logior xlib:XIMPreeditNothing
                                                  xlib:XIMStatusNothing)))
                   (make-instance 'xim:XICATTRIBUTE
                                  :id 1
                                  :length 4
                                  :value [0 0 0 0]) ;FIXME: uim/xim/ximic.cpp
                   (make-instance 'xim:XICATTRIBUTE
                                  :id 2
                                  :length 4
                                  :value [0 0 0 0])))) ;FIXME: same as above
             (exim--send (make-instance 'xim:get-ic-values-reply
                                        :im-id (slot-value obj 'im-id)
                                        :ic-id (slot-value obj 'ic-id)
                                        :length nil
                                        :ic-attributes ic-attributes)
                         connection client-window server-window)))
          ((= opcode xim:opcode:set-ic-focus)
           (exim--log "SET-IC-FOCUS")
           ;; All input contexts are the same.
           )
          ((= opcode xim:opcode:unset-ic-focus)
           (exim--log "UNSET-IC-FOCUS")
           ;; All input contexts are the same.
           )
          ((= opcode xim:opcode:forward-event)
           (exim--log "FORWARD-EVENT")
           (let ((im-func (with-current-buffer (window-buffer) ;essential
                            input-method-function)) ;save for future use
                 (obj (make-instance 'xim:forward-event))
                 key-event event)
             (xcb:unmarshal obj data)
             ;; We do not check the flag slot
             ;; Convert X key event to Emacs key event
             (let ((xcb:lsb xim:lsb))   ;in client's byte-order
               (setq key-event (make-instance 'xcb:KeyPress)))
             (xcb:unmarshal key-event (slot-value obj 'event))
             (with-slots (detail state) key-event
               (setq event (xcb:keysyms:keycode->keysym
                            (plist-get exim--internal 'connection)
                            detail state)
                     event (when event
                             (xcb:keysyms:keysym->event event state))))
             (if (plist-get exim--internal 'event-pending)
                 ;; All events should be forwarded to Emacs frame
                 (when event
                   (push event unread-command-events))
               (plist-put exim--internal 'event-pending t)
               (if (or (not im-func)      ;no active input method
                       (eq im-func 'list) ;the default method
                       (not event)        ;invalid key
                       ;; Select only printable keys
                       (not (integerp event)) (> #x20 event) (< #x7e event))
                   (with-slots (im-id ic-id serial-number event) obj
                     (exim--send (make-instance 'xim:forward-event
                                                :im-id im-id
                                                :ic-id ic-id
                                                :flag xim:commit-flag:synchronous
                                                :serial-number serial-number
                                                :event event)
                                 connection client-window server-window))
                 (with-temp-buffer ;in case buffer is read-only (e.g. EXWM mode)
                   (let* ((input-method-use-echo-area t) ;show key strokes
                          (result (encode-coding-string
                                   (concat (funcall im-func event))
                                   ;; Also works for portable character encoding
                                   'compound-text-with-extensions)))
                     (exim--send (make-instance 'xim:commit-x-lookup-chars
                                                :im-id (slot-value obj 'im-id)
                                                :ic-id (slot-value obj 'ic-id)
                                                :flag (logior xim:commit-flag:synchronous
                                                              xim:commit-flag:x-lookup-chars)
                                                :length (length result)
                                                :string result)
                                 connection client-window server-window))))
               (plist-put exim--internal 'event-pending nil))))
          ((= opcode xim:opcode:sync)
           (exim--log "SYNC")
           (let ((obj (make-instance 'xim:sync)))
             (xcb:unmarshal obj data)
             (exim--send (make-instance 'xim:sync-reply
                                        :im-id (slot-value obj 'im-id)
                                        :ic-id (slot-value obj 'ic-id))
                         connection client-window server-window)))
          ((= opcode xim:opcode:sync-reply)
           (exim--log "SYNC-REPLY"))
          ((= opcode xim:opcode:reset-ic)
           (exim--log "RESET-IC")
           ;; No context-specific data saved
           (let ((obj (make-instance 'xim:reset-ic)))
             (xcb:unmarshal obj data)
             (exim--send (make-instance 'xim:reset-ic-reply
                                        :im-id (slot-value obj 'im-id)
                                        :ic-id (slot-value obj 'ic-id)
                                        :length 0
                                        :string "")
                         connection client-window server-window)))
          ((or (= opcode xim:opcode:str-conversion-reply)
               (= opcode xim:opcode:preedit-start-reply)
               (= opcode xim:opcode:preedit-caret-reply))
           ;; No preedit support
           (exim--send (make-instance 'xim:error
                                      :im-id 0 :ic-id 0
                                      :flag xim:error-flag:invalid-both
                                      :error-code xim:error-code:bad-something
                                      :length 0 :type 0 :detail nil)
                       connection client-window server-window))
          (t
           (exim--log "[EXIM] Bad protocol")
           (exim--send (make-instance 'xim:error
                                      :im-id 0 :ic-id 0
                                      :flag xim:error-flag:invalid-both
                                      :error-code xim:error-code:bad-protocol
                                      :length 0 :type 0 :detail nil)
                       connection client-window server-window)))))

(defun exim--send (request connection client-window _server-window)
  "Send an XIM request REQUEST via connection CONNECTION."
  (let ((data (xcb:marshal request))
        event property)
    (if (>= 20 (length data))
        ;; Short request
        (progn
          (setq event
                (make-instance
                 'xcb:ClientMessage
                 :format 8 :window client-window
                 :type (plist-get exim--internal '_XIM_PROTOCOL)
                 :data (make-instance
                        'xcb:ClientMessageData
                        :data8 (append data
                                       (make-list (- 20 (length data)) 0)))))
          (xcb:+request connection
              (make-instance 'xcb:SendEvent
                             :propagate 0 :destination client-window
                             :event-mask xcb:EventMask:NoEvent
                             :event (xcb:marshal event connection))))
      ;; Long request
      (setq property (plist-get exim--internal 'property-index))
      (plist-put exim--internal 'property-index (1+ property))
      (setq property (format "_EXIM_%d" property)
            property (slot-value
                      (xcb:+request-unchecked+reply connection
                          (make-instance 'xcb:InternAtom
                                         :only-if-exists 0
                                         :name-len (length property)
                                         :name property))
                      'atom))
      (xcb:+request connection
          (make-instance 'xcb:ChangeProperty
                         :mode xcb:PropMode:Append :window client-window
                         :property property :type xcb:Atom:STRING :format 8
                         :data-len (length data) :data data))
      (setq event (make-instance
                   'xcb:ClientMessage
                   :format 32 :window client-window
                   :type (plist-get exim--internal '_XIM_PROTOCOL)
                   :data (make-instance 'xcb:ClientMessageData
                                        :data32 (list (length data)
                                                      property 0 0 0))))
      (xcb:+request connection
          (make-instance 'xcb:SendEvent
                         :propagate 0 :destination client-window
                         :event-mask xcb:EventMask:NoEvent
                         :event (xcb:marshal event connection))))
    (xcb:flush connection)))

(defun exim-start ()
  "Start EXIM."
  (unless (plist-get exim--internal 'connection)
    (add-hook 'kill-emacs-hook 'exim-stop)
    (let* ((connection (xcb:connect-to-socket))
           (root (slot-value
                  (elt (slot-value (xcb:get-setup connection) 'roots) 0)
                  'root))
           (window (xcb:generate-id connection))
           (XIM_SERVERS (slot-value
                         (xcb:+request-unchecked+reply connection
                             (make-instance 'xcb:InternAtom
                                            :only-if-exists 0
                                            :name-len (length "XIM_SERVERS")
                                            :name "XIM_SERVERS"))
                         'atom))
           (server (slot-value
                    (xcb:+request-unchecked+reply connection
                        (make-instance 'xcb:InternAtom
                                       :only-if-exists 0
                                       :name-len (length "@server=exim")
                                       :name "@server=exim"))
                    'atom))
           (LOCALES (slot-value
                     (xcb:+request-unchecked+reply connection
                         (make-instance 'xcb:InternAtom
                                        :only-if-exists 0
                                        :name-len (length "LOCALES")
                                        :name "LOCALES"))
                     'atom))
           (TRANSPORT (slot-value
                       (xcb:+request-unchecked+reply connection
                           (make-instance 'xcb:InternAtom
                                          :only-if-exists 0
                                          :name-len (length "TRANSPORT")
                                          :name "TRANSPORT"))
                       'atom)))
      (set-process-query-on-exit-flag (slot-value connection 'process) nil)
      (plist-put exim--internal 'connection connection)
      (plist-put exim--internal 'root root)
      (plist-put exim--internal 'window window)
      (plist-put exim--internal 'LOCALES LOCALES)
      (plist-put exim--internal 'TRANSPORT TRANSPORT)
      (plist-put exim--internal '_XIM_XCONNECT
                 (slot-value
                  (xcb:+request-unchecked+reply connection
                      (make-instance 'xcb:InternAtom
                                     :only-if-exists 0
                                     :name-len (length "_XIM_XCONNECT")
                                     :name "_XIM_XCONNECT"))
                  'atom))
      (plist-put exim--internal '_XIM_PROTOCOL
                 (slot-value
                  (xcb:+request-unchecked+reply connection
                      (make-instance 'xcb:InternAtom
                                     :only-if-exists 0
                                     :name-len (length "_XIM_PROTOCOL")
                                     :name "_XIM_PROTOCOL"))
                  'atom))
      ;; Initialize xcb:keysyms
      (xcb:keysyms:init connection)
      ;; Listen to SelectionRequest event for connection establishment
      (xcb:+event connection 'xcb:SelectionRequest 'exim--on-SelectionRequest)
      ;; Listen to ClientMessage event on IMS window for new XIM connection.
      (xcb:+event connection 'xcb:ClientMessage 'exim--on-ClientMessage-0)
      ;; Create an event window
      (xcb:+request connection
          (make-instance 'xcb:CreateWindow
                         :depth 0 :wid window :parent root
                         :x 0 :y 0 :width 1 :height 1 :border-width 0
                         :class xcb:WindowClass:InputOutput :visual 0
                         :value-mask xcb:CW:OverrideRedirect
                         :override-redirect 1))
      ;; Set the selection owner
      (xcb:+request connection
          (make-instance 'xcb:SetSelectionOwner
                         :owner window :selection server
                         :time xcb:Time:CurrentTime))
      ;; Set XIM_SERVERS property on the root window
      (xcb:+request connection
          (make-instance 'xcb:ChangeProperty
                         :mode xcb:PropMode:Prepend :window root
                         :property XIM_SERVERS :type xcb:Atom:ATOM :format 32
                         :data-len 1
                         :data (funcall
                                (if xcb:lsb 'xcb:-pack-u4-lsb 'xcb:-pack-u4)
                                server)))
      (xcb:flush connection))))

(defun exim-stop ()
  "Stop EXIM."
  ;; Destroy IMS communication windows and close connections
  (dolist (i (cl-loop for (key val) on exim--internal by 'cddr
                      when (integerp key) collect val))
    (let ((connection (elt i 0))
          (window (elt i 1)))
      (xcb:+request connection
          (make-instance 'xcb:DestroyWindow :window window))
      (xcb:flush connection)
      (xcb:disconnect connection)))
  ;; Destroy the IMS window and close the connection
  (let ((connection (plist-get exim--internal 'connection)))
    (when connection
      (xcb:+request connection
          (make-instance 'xcb:DestroyWindow
                         :window (plist-get exim--internal 'window)))
      (xcb:flush connection)
      (xcb:disconnect connection))))



(provide 'exim)

;;; exim.el ends here
