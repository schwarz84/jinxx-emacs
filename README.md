# Jinxx Emacs

**Minimalista. Modular. Científico.**

Configuración de Emacs optimizada para ciencia de datos, escritura técnica y desarrollo en Python.  
Arquitectura modular en archivos `.org` y gestor de paquetes `straight.el` para instalación limpia y reproducible.

---

## Características destacadas

- Arranque rápido: `straight.el` + optimizaciones de GC (`gcmh`) + configuración diferida.
- Modularidad: cada área (núcleo, paquetes, UI, Org, datos) vive en su propio `.org`.
- Flujo de trabajo data-science: Python, R/Julia (opcional), SQL, Jupyter desde Org.
- Productividad sin adornos: Ivy/Counsel/Swiper, which-key, edición múltiple, navegación con `avy`.
- Reproducible: versiones congeladas con `straight-freeze-versions` (y rollback con `straight-thaw-versions`).
- Entornos por proyecto: integración con `direnv` vía `envrc`.

---

## Estructura del proyecto

```text
jinxx_emacs/
├── early-init.el
├── init.el
├── README.md
├── LICENSE.txt
├── assets/
│   ├── logo_128.png
│   ├── logo_256.png
│   ├── logo_512.png
│   ├── logo_800.png
│   └── logo_1024.png
├── snippets/                   # opcional (yasnippet)
├── straight/                   # generada por straight.el al congelar versiones
│   └── versions/
│       └── default.el          # único archivo a versionar dentro de /straight
└── config/
    ├── core.org
    ├── packages.org
    ├── ui.org
    ├── keybindings.org
    ├── functions.org
    ├── orgmode.org
    └── data.org
```
Nota: `straight/versions/default.el` se crea al ejecutar `M-x straight-freeze-versions`.

---

## Requisitos externos (mínimos)

- Emacs 29+
- Git
- Python 3.x (y `pip`/`venv`)
- `ripgrep` (para `counsel-rg`)
- `direnv` (para `envrc` y entornos por proyecto)
- Opcional: R y/o Julia
- Opcional: Docker
- Opcional: `libvterm` (para `vterm`)

**Dependencias Python recomendadas (para data/ML):**
```bash
pip install jupyter ipykernel pyright black isort ruff sqlparse
# kernel nombrado (opcional):
python -m ipykernel install --user --name jinxx-py
```
**R (opcional):**
```r
install.packages("languageserver")
```
**Julia (opcional, REPL):**
```julia
using Pkg; Pkg.add(["LanguageServer","SymbolServer","JSON","StaticLint"])
```

---

## Ubicación de paquetes por módulo

### `config/core.org` — Núcleo y base operativa

**Objetivo:** instalación y rendimiento; dejar lo esencial listo desde el minuto 1.

**Gestor & orquestación**
- `straight.el` (bootstrap)
- `use-package` (base de todo)

**Rendimiento & limpieza**
- `gcmh` (GC más suave)
- `no-littering` (estructura de archivos limpia)

**Descubribilidad & búsqueda (setup mínimo)**
- `which-key` (activado con retardo bajo)
- `ivy` + `counsel` + `swiper` (activados; sin tunning fino)
- `ivy-prescient` (ordenamiento por uso; persistencia)

> La configuración avanzada continúa en `packages.org`.

---

### `config/packages.org` — Paquetes funcionales (config completa)

**Minibuffer, búsqueda y ayuda (extensión)**
- `which-key` (tweaks)
- `ivy`/`counsel`/`swiper`/`ivy-prescient` (filtros, formatos, atajos extra)
- `avy` (navegación ultrarrápida) — elegido

**Proyectos, archivos y entornos**
- `projectile` + `counsel-projectile` — elegidos
- `envrc` (integración con `direnv`) — elegido
- `dirvish` — opcional (Dired vitaminado; por defecto Dired nativo)
- `treemacs` + `treemacs-projectile` — opcional (más pesado; por defecto off)
- `counsel-rg` (usa `ripgrep`)

**Edición productiva**
- `multiple-cursors`, `expand-region`, `yasnippet` + `yasnippet-snippets` — elegidos
- `vundo` — elegido (simple); `undo-tree` — alternativa
- `editorconfig`, `comment-dwim-2`, `move-text` — opcionales

**Autocompletado y LSP**
- `eglot` — elegido (LSP nativo, liviano)
- `corfu` — elegido (UI simple)
- `cape` — capfs extra
- `kind-icon` — opcional
- Alternativa no predeterminada: `lsp-mode` + `lsp-ui` + `company`

**Diagnóstico y formateo**
- `apheleia` — elegido (formateo unificado asíncrono)
- Si usás `lsp-mode`: `flycheck`. Con `eglot`: preferir `flymake` builtin + linters vía LSP

**Git**
- `magit` — elegido
- `diff-hl` — opcional
- `forge` — opcional

**Terminal y HTTP**
- `vterm` — elegido (si hay `libvterm`); alternativa mínima: `eshell`
- `restclient` o `verb` — opcional

**SQL y datos**
- `sql-mode` (builtin) + `sqlformat` + `sql-indent` — elegido
- `csv-mode`, `yaml-mode`, `toml-mode`, `json-mode`
- `protobuf-mode` — opcional
- `markdown-mode` — opcional

**Docker y DevOps**
- `docker.el`, `dockerfile-mode` — elegidos
- `docker-tramp` — opcional

---

### `config/ui.org` — Interfaz mínima
- `gruvbox-theme` — elegido
- `ligature` — opcional (si tu fuente soporta ligaduras)
- `dashboard` — opcional, con carga diferida
- Sin `all-the-icons`/`nerd-icons` por defecto

---

### `config/keybindings.org` — Atajos globales
- Navegación (`ivy/counsel`, `projectile`, `magit`)
- Saltos rápidos con `avy`
- Edición (`multiple-cursors`, `expand-region`, `yasnippet`)
- Gestión de buffers/ventanas
- Criterio: no pisar bindings clásicos salvo mejoras claras

---

### `config/functions.org` — Funciones utilitarias
- Abrir proyectos, búsquedas, limpieza de temporales
- Comandos para flujos repetidos (ej.: ejecutar notebook, formatear buffer)

---

### `config/orgmode.org` — Org + Jupyter
- Org base (agenda mínima, exportaciones)
- `jupyter` (emacs-jupyter) + `ob-jupyter` + `ob-async`
- `ox-pandoc` — opcional
- Estética opcional: `org-bullets`/`org-superstar` (off por defecto)

> `emacs-jupyter` requiere Jupyter y kernels instalados en el sistema.

---

### `config/data.org` — Lenguajes y entornos de datos

**Python**
- builtin `python-mode` + `eglot`
- `pyvenv` (entornos)
- Formateo/lint:
  - Preferencia: `apheleia` con `black`/`isort`/`ruff`
  - Alternativa: `ruff-lsp`
- Opcional: `poetry`

**R**
- `ess` + `ob-R` (opcional)

**Julia**
- `julia-mode` + `eglot` (opcional)
- `julia-repl` (opcional)

**SQL**
- `sql-mode` + `sqlformat`

---

## Instalación rápida (resumen)

1. Cloná el repo en `~/.emacs.d/` o ajustá `init.el` para cargar desde `jinxx_emacs/`.
2. Abrí Emacs: `straight.el` instalará `use-package` y el resto al volar.
3. Instalá `direnv` si vas a usar entornos por proyecto y habilitalo en tu shell.
4. Cuando esté estable:
   ```elisp
   M-x straight-freeze-versions
   ```
   y versioná `straight/versions/default.el`.

**Rollback:** si una actualización rompe algo, `M-x straight-thaw-versions`.

**Server:** viene habilitado; podés abrir desde la terminal con `emacsclient -c`.

---

## Estado actual y próximos pasos

- `core.org` implementado (bootstrap + base mínima).
- Completar `packages.org` con el starter pack (incluye `envrc` y `avy`).
- Probar arranque en limpio.
- Congelar versiones (`straight-freeze-versions`).
- Documentar atajos y flujos data-science.

---

## Créditos

Inspirado por configuraciones minimalistas de Emacs y afinado para ciencia de datos por Carlos.
