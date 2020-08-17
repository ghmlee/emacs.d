default: build

build:
	git clone https://github.com/jonathanchu/atom-one-dark-theme.git ./packages/atom-one-dark-theme || true
	git clone https://github.com/rust-lang/rust-mode.git ./packages/rust-mode || true
	git clone https://github.com/swift-emacs/swift-mode.git ./packages/swift-mode || true
	git clone https://github.com/dominikh/go-mode.el.git ./packages/go-mode || true
	git clone https://github.com/tamzinblake/js3-mode.git ./packages/js3-mode || true
	git clone https://github.com/fgallina/multi-web-mode.git ./packages/multi-web-mode || true
	git clone https://github.com/jrblevin/markdown-mode.git ./packages/markdown-mode || true
	git clone https://github.com/yoshiki/yaml-mode.git ./packages/yaml-mode || true
	git clone https://github.com/mooz/js2-mode.git ./packages/js2-mode || true
	git clone https://github.com/felipeochoa/rjsx-mode.git ./packages/rjsx-mode || true
	git clone https://github.com/magnars/s.el.git ./packages/s || true
	git clone https://github.com/spotify/dockerfile-mode.git ./packages/dockerfile-mode || true
	git clone https://github.com/magnars/dash.el.git ./packages/dash || true
	git clone https://github.com/groovy-emacs-modes/groovy-emacs-modes.git ./packages/groovy-mode || true
	git clone https://github.com/prettier/prettier-emacs.git ./packages/prettier-js || true
	git clone https://github.com/jwiegley/emacs-async.git ./packages/async || true
	git clone https://github.com/magit/transient.git ./packages/transient || true
	git clone https://github.com/magit/with-editor.git ./packages/with-editor || true
	git clone https://github.com/magit/magit.git ./packages/magit || true
	git clone https://github.com/purcell/exec-path-from-shell.git ./packages/exec-path-from-shell || true
	git clone https://github.com/integral-dw/org-bullets.git ./packages/org-bullets || true
	git clone https://github.com/bbatsov/projectile.git ./packages/projectile || true
	git clone https://depp.brause.cc/eyebrowse.git ./packages/eyebrowse || true
	git clone https://github.com/abo-abo/swiper.git ./packages/swiper || true
	git clone https://github.com/ericdanan/counsel-projectile.git ./packages/counsel-projectile || true
	git clone https://github.com/victorhge/iedit.git ./packages/iedit || true
	git clone https://github.com/mhayashi1120/Emacs-wgrep.git ./packages/wgrep || true
	git clone https://github.com/purcell/ibuffer-vc.git ./packages/ibuffer-vc || true
	git clone https://github.com/manateelazycat/multi-term.git ./packages/multi-term || true
	git clone https://github.com/company-mode/company-mode.git ./packages/company || true
	git clone https://github.com/flycheck/flycheck.git ./packages/flycheck || true
	git clone https://github.com/justbur/emacs-which-key.git ./packages/which-key || true
	git clone https://github.com/tarsius/minions.git ./packages/minions || true

clean:
	rm -rf ./packages/
