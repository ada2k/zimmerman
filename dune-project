(lang dune 3.12)

(generate_opam_files true)

(package
 (name zimmerman)
 (synopsis "tiny router for cohttp-eio")
 (description "A lightweight, opium-inspired router and middleware stack for EIO and Cohttp")
 (depends cohttp (cohttp-eio (>= 6.0.0~beta2)) logs))
