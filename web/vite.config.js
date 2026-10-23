import { defineConfig } from "vite";

export default defineConfig({
  // Relative base => the build works under any GitHub Pages subpath.
  base: "./",
  // The training worker imports ES modules (Fable library).
  worker: { format: "es" },
});
