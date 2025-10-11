# Jinxx Emacs

**Minimalista · Modular · Científico**

Una configuración de Emacs optimizada para ciencia de datos, desarrollo de software y escritura técnica. Su arquitectura modular en archivos `.org` y el gestor de paquetes `straight.el` garantizan una instalación limpia, reproducible y personalizable.

---

## Estructura del Proyecto

```text
jinxx_emacs/
├── early-init.el         # Optimización de arranque temprano
├── init.el               # Punto de entrada y cargador de módulos
├── README.md             # Esta guía
├── LICENSE.txt
├── assets/
│   └── ... (logos)
├── snippets/             # Snippets personales para yasnippet
└── config/
    ├── core.org          # Base: straight.el, use-package, gestión de archivos
    ├── packages.org      # Paquetes para UI, edición, Git y formatos de datos
    ├── ui.org            # Apariencia: tema, fuentes, modeline
    ├── keybindings.org   # Atajos de teclado globales
    ├── functions.org     # Funciones helper personalizadas
    ├── orgmode.org       # Configuración específica para Org-mode
    └── data.org          # ¡El motor! Jupyter, R (ESS), Julia, Notebooks
```

-----

## Requisitos Externos

Para que Jinxx Emacs funcione a pleno rendimiento, se necesitan algunas dependencias. Las siguientes instrucciones son para **Arch Linux** (o equivalentes).

### Dependencias Esenciales

```bash
sudo pacman -S emacs git ripgrep libvterm tree-sitter-cli
```

- **emacs**: El editor (versión 29+ recomendada).
- **git**: Requerido por `straight.el`.
- **ripgrep (`rg`)**: Potencia la búsqueda en proyectos con `counsel-rg`.
- **libvterm**: Biblioteca nativa para la emulación de terminal de alto rendimiento con `vterm`.
- **tree-sitter-cli**: Necesario para compilar las gramáticas de Tree-sitter.

### Otras plataformas

#### Debian/Ubuntu

```bash
sudo apt update
sudo apt install emacs git ripgrep libvterm-dev build-essential cmake curl
```

- `libvterm-dev` y `cmake` son necesarios para compilar el módulo nativo de `vterm`.
- Si tu repositorio de distribución no incluye Emacs 29+, añade el PPA oficial: `sudo add-apt-repository ppa:ubuntu-elisp/ppa`.

#### Fedora

```bash
sudo dnf install emacs git ripgrep libvterm-devel cmake make gcc-c++
```

- Fedora ya incluye Emacs reciente; si usas Silverblue/Kinoite instala los paquetes con `rpm-ostree`.

#### macOS (Homebrew)

```bash
brew install emacs-plus@29 git ripgrep cmake libtool coreutils
brew install --cask iterm2   # Opcional, para un terminal con soporte pleno de 24 bits
```

- Reemplaza `emacs-plus@29` por la fórmula que prefieras (`emacs-mac`, etc.).
- Tras la instalación ejecuta `brew doctor` para asegurarte de que Homebrew exporta los binarios en tu `PATH`.

#### Windows 10/11 (MSYS2/Chocolatey)

```powershell
choco install emacs git ripgrep cmake
```

1. Instala [MSYS2](https://www.msys2.org/) y, desde la terminal `MSYS2 UCRT64`, ejecuta:
   ```bash
   pacman -S --needed base-devel mingw-w64-ucrt-x86_64-toolchain mingw-w64-ucrt-x86_64-libvterm
   ```
2. Asegúrate de que `C:\msys64\ucrt64\bin` esté en tu variable de entorno `PATH` para que Emacs encuentre `libvterm`.

> 💡 **Consejo:** Si trabajas en entornos restringidos donde no puedes instalar paquetes del sistema, instala `ripgrep` y `tree-sitter-cli` dentro de un entorno Conda o virtualenv y apunta `exec-path` a ese directorio.

### Stack de Ciencia de Datos

#### Python (Recomendado con Miniconda/Anaconda)

```bash
# Se recomienda gestionar los entornos con Conda.
# Instala las librerías en un entorno de conda dedicado:
conda create -n datascience python=3.10
conda activate datascience
pip install jupyterlab notebook ipykernel debugpy basedpyright black isort
```

- **jupyterlab, notebook, ipykernel**: Para la experiencia de notebook (`emacs-jupyter`, `EIN`).
- **debugpy**: Para la depuración de código Python (`dap-mode`).
- **basedpyright**: Servidor LSP recomendado para Python.
- **black, isort**: Formateadores de código.

Establece la variable de entorno `CONDA_HOME` para señalar tu instalación de Conda si no resides en `~/miniconda3` o `~/anaconda3`. Por ejemplo: `export CONDA_HOME=$HOME/mambaforge`. La configuración detectará automáticamente el valor y lo usará tanto para `conda-anaconda-home` como para `conda-env-home-directory`.


#### R y Julia (Opcional)

```bash
# R y su Language Server
sudo pacman -S r r-languageserver

# Julia
sudo pacman -S julia
# Dentro de Julia, instala los paquetes del LSP:
julia -e 'using Pkg; Pkg.add(["LanguageServer", "SymbolServer"])'
```

-----

## Características Clave

### Arquitectura

- **Literate & Modular**: Configuración escrita en archivos `.org` que se "tanglean" a `.el` bajo demanda, facilitando el mantenimiento.
- **Reproducible**: Usa `straight.el` para gestionar paquetes directamente desde sus repositorios, permitiendo fijar versiones.
- **Rendimiento**: `early-init.el` optimiza el arranque al mínimo, y `gcmh` gestiona la memoria de forma inteligente.

### Entorno de Desarrollo (IDE)

- **Autocompletado Inteligente**: `eglot` como cliente LSP para análisis de código, combinado con `cape` para completado contextual.
- **Errores en Tiempo Real**: `flycheck` se integra con `eglot` para subrayar errores y advertencias directamente en el buffer.
- **Navegación Eficiente**: `ivy`, `counsel`, y `swiper` para una interacción fluida con el minibuffer.
- **Gestión de Proyectos**: `projectile` para la lógica de proyectos y `treemacs` como explorador de archivos visual.
- **Control de Versiones Superior**: `magit` para una interfaz de Git completa y `diff-hl` para visualizar cambios en el margen.
- **Terminal Integrada**: `vterm` como terminal rápida, con un atajo para abrirla en la raíz del proyecto actual.
- **Gestión de Entornos**: Se integra con **Conda** para detectar y activar automáticamente el entorno de Python correcto para cada proyecto.
- **Trabajo Remoto**: Configuración lista para usar con **TRAMP** (`ssh` por defecto), soporte para contenedores mediante el método nativo `tramp-container` (o `docker-tramp` en Emacs antiguos) y reutilización de credenciales con `ssh-agency`.

### Flujos de Trabajo para Ciencia de Datos

1.  **Notebooks en Org-mode (Recomendado)**: A través de `emacs-jupyter`, ejecuta bloques de código en kernels de Jupyter. Los resultados (tablas, gráficos) se muestran directamente en el buffer de Org.
2.  **Desarrollo REPL-Driven**: Soporte de primera clase para **R (ESS)** y **Julia (julia-repl)**, permitiendo enviar código desde un script a una consola interactiva.
3.  **Compatibilidad con Notebooks Nativos**: Abre, edita y trabaja con archivos `.ipynb` directamente en Emacs usando `ein`, para colaborar con equipos que usan el formato estándar de Jupyter.

-----

## Atajos Clave

| Acción | Tecla | Paquete |
|---|---|---|
| Ejecutar comando | `M-x` | `counsel` |
| Buscar archivo en proyecto | `C-c p f` | `counsel-projectile` |
| Buscar texto en proyecto | `C-c p g` | `counsel-projectile` |
| Cambiar de buffer | `C-x b` | `counsel` |
| Buscar en buffer actual | `C-s` | `swiper` |
| Magit Status | `C-x g` | `magit` |
| Treemacs (abrir/seleccionar/ocultar) | `M-1` | `treemacs` |
| Terminal en raíz del proyecto | `C-c t t` | `vterm-toggle` |
| Ejecutar bloque de código (Org) | `C-c C-c` | `org-babel` / `jupyter` |
| Alinear columnas CSV/TSV | `C-c a` | `csv-mode` |

-----

## Instalación Rápida

1.  **Asegúrate de tener todas las [dependencias externas](#requisitos-externos) instaladas.**
2.  **Clona el repositorio:**
    ```bash
    # Haz una copia de seguridad de tu configuración actual si existe
    # mv ~/.emacs.d ~/.emacs.d.bak
    git clone <URL_DEL_REPO> ~/.emacs.d
    ```
3.  **Primer Arranque:**
    - Abre Emacs. `straight.el` descargará e instalará todas las dependencias. La primera vez puede tardar varios minutos.
    - La configuración instalará automáticamente las gramáticas de Tree-sitter que faltan.

¡Listo! Ya tienes un entorno de desarrollo y ciencia de datos de primer nivel.
