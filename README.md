# Jinxx Emacs

**Minimalista · Modular · Científico**

Configuración de Emacs pensada para ciencia de datos, escritura técnica y desarrollo.
Arquitectura modular en archivos `.org` + gestor de paquetes **straight.el** para instalación limpia y reproducible.

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

* **Emacs 29+** (probado en 30.2)
* **Git** (straight.el lo necesita)
* **ripgrep** (`counsel-rg`)
* **libvterm** (para `vterm`)

> Opcionales para data: **Python 3** (+ `pip`/`venv`), **R**, **Julia**, **Docker**.

### Sugerencias de instalación

**Python (formateo y notebooks):**

```bash
pip install jupyter ipykernel black isort ruff
# kernel nombrado (opcional)
python -m ipykernel install --user --name jinxx-py
```

**R (opcional):**

```r
install.packages("languageserver")
```

**Julia (opcional):**

```julia
using Pkg; Pkg.add(["LanguageServer","SymbolServer","JSON","StaticLint"])
```

---

## ¿Cómo carga Jinxx Emacs?

### `early-init.el` — Optimización temprana

* UI mínima (sin menú, barras, ni diálogos), borde interno 12, arranque **maximizado**.
* GC agresivo durante el boot, `file-name-handler-alist` congelado.
* Compilación nativa activada (si está disponible) con warnings silenciosos en arranque.
* Tuning de redisplay y procesos: `read-process-output-max` 4 MB, scroll fluido.

### `init.el` — Núcleo de arranque

* Restaura GC y handlers post-arranque y muestra el **tiempo de carga**.
* Carga `custom.el` separado.
* Importa `$PATH` desde tu shell de login.
* **Arquitectura modular**:

  * Si existe `config/<módulo>.el` y está actualizado, lo carga.
  * Si no, **tanglea** el `config/<módulo>.org` → `el` con Org y lo carga.
* Inicia `server` para `emacsclient`.

> Módulos activos: `core`, `packages`, `ui`, `functions`, `keybindings`, `orgmode`, `data`.

---

## Módulos y funcionalidades

### `config/core.org` — Bootstrap y base

* **straight.el + use-package** (defer por defecto).
* Rendimiento: **gcmh** (GC en idle) y **no-littering** (archivos en `~/.emacs.d/{etc,var}/`).
* Autosaves y listas en `var/auto-save/` y `var/auto-save-list/`.
* Historiales centralizados: `save-place`, `savehist`, `recentf`, `bookmarks`.
* Comportamiento general: backups versionados, `auto-revert`, `electric-pair` global, UTF‑8.

### `config/packages.org` — Stack elegido

**Minibuffer, búsqueda y ayuda**

* `which-key` — guía de atajos.
* `ivy` + `counsel` — completado y comandos enriquecidos.
* `swiper` — búsqueda incremental por buffer.
* `ivy-rich` — columnas extra en menús de Ivy/Counsel.
* `smex` — ranking por uso en `M-x`.
* `prescient` + `ivy-prescient` — orden por frecencia.
* `helpful` — ayuda mejorada.
* `whitespace` — visualización sutil de espacios.
* `wgrep` — edita resultados de `rg`/`grep` y aplica a archivos.
* `expand-region` — selección semántica.
* `winner` — deshacer/rehacer layouts de ventanas.

**Proyectos, árbol y sesión**

* `projectile` + `counsel-projectile` — navegación de proyectos (con cache y `alien` indexing).
* `treemacs` (+ `treemacs-projectile`, `treemacs-nerd-icons`) — vista de árbol a la izquierda.
* `desktop` — sesión persistente (`var/desktop/`).

**Terminal y panel**

* `vterm` + `multi-vterm` + `vterm-toggle` — terminal rápida en panel inferior fijo (30%).

**Git**

* `magit` — interfaz Git completa (todo en la misma ventana, diffs aparte).
* `forge` — issues/PRs (opcional, carga al abrir Magit).
* `magit-todos` — lista de TODO/FIXME desde `magit-status`.
* `diff-hl` — marcas en el margen + navegación/revert por *hunk*.

**Edición productiva**

* `crux` — utilidades (duplicar línea/region, smart-kill, etc.).
* `multiple-cursors` — múltiples cursores (archivo de listas en `var/.mc-lists.el`).
* `avy` — saltos por char/word/line.
* `smartparens` (estricto en `prog-mode` y `org-mode`) + `show-smartparens`.
* `yasnippet` + `yasnippet-snippets` — snippets (directorio `snippets/` + comunidad).
* `apheleia` — formateo **asíncrono** estable al punto (Python: `isort` + `black`).
* `move-text` — mover línea/selección arriba/abajo.

**Lenguajes**

* `markdown-mode` — `.md` (usa `gfm-mode`).

### `config/ui.org` — Interfaz mínima

* Fuente **JetBrains Mono** si está instalada (110%).
* Modeline compacto (`column-number-mode`, `minions`).
* Tema **gruvbox-dark-hard**.
* Ligaduras con `ligature.el` (global).
* Paréntesis y delimitadores: `show-paren-mode`, `rainbow-delimiters`.
* Números de línea **relativos** en `prog-mode`.
* Org: selección con `Shift`.

### `config/keybindings.org` — Atajos

* `ESC` → cancelar en todo el sistema y minibuffer.
* Movimiento de ventanas: **Ctrl+←/→/↑/↓** (windmove).
* `C-c w t` → *toggle* de `whitespace-mode`.
* **Org (edición de bloques)**:

  * `C-c '`: `jinxx/org-edit-src-current-window` (edita en **ventana actual**).
  * `C-c "`: `jinxx/org-edit-src-default` (comportamiento por defecto de Org).
* **Treemacs**: `M-1` → `jinxx/treemacs-smart` (abrir/seleccionar/ocultar según contexto).
* **Terminal**:

  * `C-c t t` → `jinxx/vterm-toggle-project` (panel inferior en root del proyecto).
  * `C-c t n` / `C-c t p` → siguiente/anterior `vterm`.
* **Git**: `C-x g` (status), `C-x M-g` (dispatch), `C-c g` (file-dispatch).
* **diff-hl**: `C-c v n/p/r` → siguiente/anterior/revert *hunk*.
* **Snippets**: `C-c y i`/`n`/`v` → insertar/nuevo/visitar snippet.
* **Multiple cursors**: `C->`/`C-<`/`C-c m l`/`b`/`e`/`a`/`r` (escape: `<escape>`).
  Alt‑clic (`M-<mouse-1>`) añade/quita cursor.
* **expand-region**: `C-=` / `C--`.
* **CRUX** (ejemplos): `C-c x d` (duplicar), `C-c x k` (smart-kill-line), `C-c x K` (kill otros buffers).

### `config/functions.org` — Helpers

* `jinxx/try-straight` — instala/carga un paquete puntual con **straight**.
* `jinxx/org-edit-src-…` — helpers para editar bloques de código en Org.
* `jinxx/treemacs-smart` — comportamiento inteligente de Treemacs según visibilidad.
* `jinxx/apheleia-skip-large-file` — evita formateo en buffers >1 MB.
* `jinxx/vterm-toggle-project` — *toggle* de `vterm` en panel, cambiando al root del proyecto.

### `config/orgmode.org` — Org base

* `org-src-tab-acts-natively t` y `org-edit-src-content-indentation 0`.

### `config/data.org`

* Marcador de posición (mensaje de carga), listo para futuras ampliaciones.

---

## Atajos clave (chuleta rápida)

| Acción                                  | Tecla                             |
| --------------------------------------- | --------------------------------- |
| Ejecutar comando                        | `M-x` (counsel-M-x)               |
| Abrir archivo                           | `C-x C-f`                         |
| Cambiar buffer                          | `C-x b`                           |
| Búsqueda por proyecto                   | `C-c p g` (counsel-projectile-rg) |
| Buscar en buffer                        | `C-s` (swiper)                    |
| Treemacs smart                          | `M-1`                             |
| Terminal proyecto                       | `C-c t t`                         |
| Magit status                            | `C-x g`                           |
| Múltiples cursores (siguiente/anterior) | `C->` / `C-<`                     |
| expand-region (expandir/contraer)       | `C-=` / `C--`                     |

> **Nota**: el mapa `C-c p …` se instala al cargar Projectile; se enlaza explícitamente el `projectile-command-map` para evitar errores de `void-variable`.

---

## Instalación rápida

1. **Clonar** en tu directorio de configuración (recomendado como `~/.emacs.d/`):

```bash
cd ~
mv ~/.emacs.d ~/.emacs.d.bak  # si existía
git clone <TU_REPO> ~/.emacs.d
```

2. **Primer arranque**: abrir Emacs y esperar a que `straight.el` descargue dependencias.

3. **Congelar versiones** (cuando todo esté estable):

```
M-x straight-freeze-versions
```

Esto crea/actualiza `straight/versions/default.el`. Versionalo en Git.

4. **Actualizar paquetes** (cuando quieras):

```
M-x straight-pull-all
M-x straight-rebuild-all
```

5. **Rollback** rápido si algo rompió:

```
M-x straight-thaw-versions
```

---

## Snippets (YAS)

* Carpeta personal: `snippets/` (prioritaria).
* Comunidad: `straight/repos/yasnippet-snippets/snippets`.
* Recargar: `M-x yas-reload-all`.

> Ejemplo útil (Org): snippet para insertar bloques `#+begin_src … / #+end_src`.

---

## Consejos y resolución de problemas

* **`counsel-rg` no encuentra `rg`** → instalá *ripgrep* en tu sistema.
* **`vterm` no abre** → asegurate de tener **emacs-libvterm** compilado con `libvterm` (dependencia nativa). En distros rolling puede requerir paquetes `*-devel`.
* **Paréntesis dobles o pelea con `electric-pair`** → `smartparens` ya desactiva `electric-pair-local-mode` donde corre.
* **Treemacs sin iconos** → `treemacs-nerd-icons` requiere tener una fuente de *nerd icons* instalada; si no, funciona sin iconos.
* **Warnings de compilación nativa** en arranque: se silencian en `early-init.el` y pasan a `warn` post‑init para que no contaminen la sesión.
* **Sesión**: `desktop-save-mode` guarda/recupera ventanas/buffers en `var/desktop/`.

---

## Roadmap (no instalado todavía)

Sugerencias para ampliar stack de ciencia de datos sin salir de Emacs:

* **Notebooks/Org-Babel**: `emacs-jupyter` + `ob-async`.
* **R/Julia**: `ess`, `julia-repl`.
* **LSP + autocompletado**: `eglot` + `corfu` + `cape` si querés UI de completado en buffer.
* **DevOps**: `docker.el`, `dockerfile-mode`, `docker-tramp`.
* **SQL/Datos**: `sqlformat`, `sql-indent`, `csv-mode`, `yaml-mode`, `toml-mode`, `json-mode`.

> La filosofía sigue siendo **ligera y reproducible**; agregá módulos y atajos sólo cuando los necesites.

---

## Créditos y licencia

Inspirado en configuraciones minimalistas de Emacs y afinado para ciencia de datos por **Carlos**.
Licencia: ver `LICENSE.txt`.

---

## Mantenimiento

* Código organizado por módulos `.org` con *tangle on‑demand*.
* `straight-check-for-modifications` = `find-when-checking` para costes de I/O mínimos.
* Evitá mezclar gestores de paquetes.
* Usá ramas para cambios grandes y **congelá** versiones al cerrar cada iteración.

¡Listo! Abrí Emacs y a trabajar.  in iconos
