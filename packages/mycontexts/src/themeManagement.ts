import { get as getValue, set as setValue } from 'idb-keyval';

export type ThemeOption = {
  id: string;
  name: string;
  type: 'standard' | 'custom';
  url?: string; // Used for custom external themes
  stylesheetId?: string; // For dynamic theme loading
};

// Standard Bootstrap themes
const standardThemes: ThemeOption[] = [
  { id: 'default', name: 'Default Bootstrap', type: 'standard' },
  { id: 'flatly', name: 'Flatly', type: 'standard' },
  { id: 'darkly', name: 'Darkly', type: 'standard' },
  { id: 'journal', name: 'Journal', type: 'standard' },
  // Add more standard themes as needed
];

// Initial custom theme setup
const initialCustomThemes: ThemeOption[] = [
  { 
    id: 'team-custom', 
    name: 'MyContexts Custom', 
    type: 'custom',
    url: 'https://example.com/custom-bootstrap-theme.min.css' // Replace with your actual URL
  }
];

export class ThemeManager {
  private static instance: ThemeManager;
  private themes: ThemeOption[] = [...standardThemes];
  private currentTheme: ThemeOption | null = null;
  
  private constructor() {
    // Private constructor for singleton
  }
  
  public static getInstance(): ThemeManager {
    if (!ThemeManager.instance) {
      ThemeManager.instance = new ThemeManager();
    }
    return ThemeManager.instance;
  }
  
  // Initialize themes from storage and defaults
  public async initialize(): Promise<void> {
    // Get custom themes from storage
    const storedCustomThemes = await getValue('customThemes') as ThemeOption[] | undefined;
    this.themes = [
      ...standardThemes,
      ...(storedCustomThemes || initialCustomThemes)
    ];
    
    // Get user's theme preference
    const storedThemeId = await getValue('selectedTheme') as string | undefined;
    if (storedThemeId) {
      const theme = this.themes.find(t => t.id === storedThemeId);
      if (theme) {
        this.currentTheme = theme;
        await this.applyTheme(theme);
      }
    } else {
      // Default theme
      this.currentTheme = this.themes[0];
      await this.applyTheme(this.currentTheme);
    }
  }
  
  // Get available themes
  public getThemes(): ThemeOption[] {
    return [...this.themes];
  }
  
  // Get current theme
  public getCurrentTheme(): ThemeOption | null {
    return this.currentTheme;
  }
  
  // Apply a theme
  public async applyTheme(theme: ThemeOption): Promise<void> {
    // Remove previous theme if it exists
    if (this.currentTheme?.stylesheetId) {
      const oldLink = document.getElementById(this.currentTheme.stylesheetId);
      if (oldLink) {
        document.head.removeChild(oldLink);
      }
    }
    
    let stylesheetId: string;
    
    if (theme.type === 'standard') {
      // Load standard theme from node_modules
      stylesheetId = `theme-${theme.id}`;
      await this.loadStylesheet(
        `https://cdn.jsdelivr.net/npm/bootswatch@5/dist/${theme.id}/bootstrap.min.css`, 
        stylesheetId
      );
    } else if (theme.type === 'custom' && theme.url) {
      // Load custom theme from URL
      stylesheetId = `theme-custom-${theme.id}`;
      await this.loadStylesheet(theme.url, stylesheetId);
    }
    
    // Update current theme
    this.currentTheme = {
      ...theme,
      stylesheetId
    };
    
    // Save user preference
    await setValue('selectedTheme', theme.id);
    
    // Notify subscribers
    document.dispatchEvent(new CustomEvent('themeChanged', { detail: theme }));
  }
  
  // Add a new custom theme
  public async addCustomTheme(name: string, url: string): Promise<ThemeOption> {
    const id = `custom-${Date.now()}`;
    const newTheme: ThemeOption = {
      id,
      name,
      type: 'custom',
      url
    };
    
    this.themes.push(newTheme);
    
    // Save to storage
    const customThemes = this.themes.filter(t => t.type === 'custom');
    await setValue('customThemes', customThemes);
    
    return newTheme;
  }
  
  // Update custom theme URL
  public async updateCustomTheme(id: string, url: string): Promise<ThemeOption | null> {
    const themeIndex = this.themes.findIndex(t => t.id === id && t.type === 'custom');
    if (themeIndex === -1) return null;
    
    this.themes[themeIndex].url = url;
    
    // Save to storage
    const customThemes = this.themes.filter(t => t.type === 'custom');
    await setValue('customThemes', customThemes);
    
    // If this is the current theme, reapply it
    if (this.currentTheme?.id === id) {
      await this.applyTheme(this.themes[themeIndex]);
    }
    
    return this.themes[themeIndex];
  }
  
  private loadStylesheet(url: string, id: string): Promise<void> {
    return new Promise((resolve, reject) => {
      const link = document.createElement('link');
      link.rel = 'stylesheet';
      link.type = 'text/css';
      link.href = url;
      link.id = id;
      link.onload = () => resolve();
      link.onerror = () => reject(new Error(`Failed to load stylesheet: ${url}`));
      document.head.appendChild(link);
    });
  }
}

// Initialize the theme manager
export const initializeThemes = async (): Promise<void> => {
  const themeManager = ThemeManager.getInstance();
  await themeManager.initialize();
};