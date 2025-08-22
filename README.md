# Jinxx Emacs

**Minimalista. Modular. Científico.**

Configuración de Emacs con la idea de que este optimizada para ciencia de datos, escritura técnica y desarrollo.
Arquitectura modular en archivos `.org` y gestor de paquetes `straight.el` para instalación limpia y reproducible.

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
---

## Requisitos externos (mínimos)

- Emacs 29+
- Git
- Python 3.x (y `pip`/`venv`)
- `ripgrep` (para `counsel-rg`)
- `direnv` (para `envrc` y entornos por proyecto)
- R y/o Julia
- Docker
- `libvterm` (para `vterm`)

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

## Configuracion

### early-init.el - Optimizacion temprana de Emacs
La función de este archivo es optimizar arranque, rendimiento y apariencia inicial.

- Arranque optimizado: menos GC, menos handlers, sin cargas extra.
- UI minimalista: sin menús, barras ni diálogos.
- Optimización para LSP/procesos grandes: buffers más grandes, scroll fluido.
- Seguridad controlada: sin cargar código dudoso, warnings relevantes visibles.
- Preparado para straight.el: gestión de paquetes.
- Compilación nativa activada: código Lisp más rápido.

### init.el - Restauración de estado
Realiza una restauración del sistema, importando los PATH base y cargando los modulos.

- Optimización post-arranque: restaura GC, handlers y muestra tiempo.
- Configuración limpia: separa custom.el.
- Entorno correcto: importa PATH de la shell.
- Carga modular inteligente: usa .el si está actualizado, tangling solo cuando hace falta.
- Ajustes generales: redefine funciones sin warnings, variables locales seguras.
- Finalización cuidada: GC, nativo con warnings, servidor Emacsclient.


### `config/core.org` — Núcleo y base operativa
Realiza instalación y maneja el rendimiento; dejar lo esencial.

- Gestión moderna de paquetes: straight.el + use-package, diferido, minimalista.
- Rendimiento: GCMH para GC, no-littering para limpieza.
- Archivos limpios: autosaves, historial, recentf y bookmarks centralizados en ~/.emacs.d/var/.
- Comodidad general: backups, respuestas cortas, auto-pairs, refresh automático.
- Universalidad: todo en UTF-8.
- Transparencia: mensaje de carga confirmada.

---

### `config/packages.org` — Paquetes funcionales (config completa)

### Minibuffer, búsqueda y ayuda
- `which-key` 
- `ivy` + `counsel` + `ivy-prescient`
- `helpful` + `swiper` + `ivi-rich`
- `avy` 
- `wgrep` 
- `bookmark` (builtin, integrado con Ivy)

### Proyectos, archivos y entornos
- `projectile` + `counsel-projectile`
- `envrc` 
- `dirvish` 

### Edición productiva
- `multiple-cursors`, `expand-region` 
- `yasnippet` + `yasnippet-snippets` 
- `vundo` 
- `editorconfig`, `comment-dwim-2`, `move-text`, `hungry-delete` 
- `hl-todo`
- `crux`

### Autocompletado y LSP
- `corfu` 
- `cape` 
- `eglot` 

### Diagnóstico y formateo
- `apheleia` 

### Git
- `magit` 
- `diff-hl` 
- `forge`

### Terminal y HTTP
- `vterm` 
- `verb` 

### SQL y datos
- `sql-mode` (builtin) + `sqlformat` + `sql-indent` 
- `csv-mode`, `yaml-mode`, `toml-mode`, `json-mode`, `markdown-mode`, `protobuf-mode`

### Docker y DevOps
- `docker.el` 
- `dockerfile-mode` 
- `docker-tramp`

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
