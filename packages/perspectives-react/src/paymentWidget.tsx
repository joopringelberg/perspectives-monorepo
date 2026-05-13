// BEGIN LICENSE
// Perspectives Distributed Runtime
// Copyright (C) 2019-2026 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.
//
// Full text of this license can be found in the LICENSE file in the projects root.
// END LICENSE
import React, { createRef } from "react";
import PerspectivesComponent from "./perspectivesComponent";

type AdyenEnvironment = "test" | "live";

interface AdyenSession {
  id: string;
  sessionData: string;
}

interface AdyenAmount {
  value: number;
  currency: string;
}

type AdyenEventHandler = (event: unknown) => void;

export interface PaymentWidgetProps {
  clientKey: string;
  environment?: AdyenEnvironment;
  session: AdyenSession;
  amount: AdyenAmount;
  reference: string;
  countryCode?: string;
  locale?: string;
  showPayButton?: boolean;
  className?: string;
  timeoutMs?: number;
  onPending?: AdyenEventHandler;
  onCompleted?: AdyenEventHandler;
  onFailed?: AdyenEventHandler;
  onError?: AdyenEventHandler;
  onTimeout?: AdyenEventHandler;
}

interface PaymentWidgetState {
  isLoading: boolean;
  error?: string;
}

interface AdyenDropin {
  mount: (element: HTMLElement) => void;
  unmount?: () => void;
}

interface AdyenCheckoutInstance {
  create: (component: "dropin", configuration: Record<string, unknown>) => AdyenDropin;
}

type AdyenCheckoutConstructor = (configuration: Record<string, unknown>) => Promise<AdyenCheckoutInstance>;

interface WindowWithAdyen extends Window {
  AdyenCheckout?: AdyenCheckoutConstructor;
}

const ADYEN_SDK_VERSION = "6.19.0";
const TEST_BASE_URL = "https://checkoutshopper-test.adyen.com/checkoutshopper";
const LIVE_BASE_URL = "https://checkoutshopper-live.adyen.com/checkoutshopper";
const adyenLoaderPromises: Partial<Record<AdyenEnvironment, Promise<void>>> = {};

function ensureStylesheet(environment: AdyenEnvironment): void {
  const styleId = `perspectives-adyen-${environment}-css`;
  if (document.getElementById(styleId)) {
    return;
  }

  const baseUrl = environment === "live" ? LIVE_BASE_URL : TEST_BASE_URL;
  const link = document.createElement("link");
  link.id = styleId;
  link.rel = "stylesheet";
  link.href = `${baseUrl}/sdk/${ADYEN_SDK_VERSION}/adyen.css`;
  document.head.appendChild(link);
}

function ensureScript(environment: AdyenEnvironment): Promise<void> {
  const scriptId = `perspectives-adyen-${environment}-js`;
  const existingScript = document.getElementById(scriptId) as HTMLScriptElement | null;
  if (existingScript) {
    if (existingScript.dataset.ready === "true") {
      return Promise.resolve();
    }

    return new Promise((resolve, reject) => {
      existingScript.addEventListener("load", () => resolve(), { once: true });
      existingScript.addEventListener("error", () => reject(new Error("Kon Adyen script niet laden.")), { once: true });
    });
  }

  const baseUrl = environment === "live" ? LIVE_BASE_URL : TEST_BASE_URL;
  return new Promise((resolve, reject) => {
    const script = document.createElement("script");
    script.id = scriptId;
    script.async = true;
    script.src = `${baseUrl}/sdk/${ADYEN_SDK_VERSION}/adyen.js`;
    script.addEventListener(
      "load",
      () => {
        script.dataset.ready = "true";
        resolve();
      },
      { once: true }
    );
    script.addEventListener("error", () => reject(new Error("Kon Adyen script niet laden.")), { once: true });
    document.body.appendChild(script);
  });
}

function loadAdyenSdk(environment: AdyenEnvironment): Promise<void> {
  if (!adyenLoaderPromises[environment]) {
    adyenLoaderPromises[environment] = (async () => {
      ensureStylesheet(environment);
      await ensureScript(environment);
    })();
  }

  return adyenLoaderPromises[environment] as Promise<void>;
}

export default class PaymentWidget extends PerspectivesComponent<PaymentWidgetProps, PaymentWidgetState> {
  private mountRef = createRef<HTMLDivElement>();
  private dropin: AdyenDropin | undefined;
  private timeoutHandle: number | undefined;

  constructor(props: PaymentWidgetProps) {
    super(props);
    this.state = { isLoading: true };
  }

  async componentDidMount(): Promise<void> {
    super.componentDidMount();
    await this.initialiseAdyen();
  }

  componentWillUnmount(): void {
    if (this.timeoutHandle !== undefined) {
      window.clearTimeout(this.timeoutHandle);
      this.timeoutHandle = undefined;
    }
    this.dropin?.unmount?.();
    this.dropin = undefined;
    super.componentWillUnmount();
  }

  private async initialiseAdyen(): Promise<void> {
    const environment = this.props.environment ?? "test";
    try {
      await loadAdyenSdk(environment);

      const adyenCheckoutConstructor = (window as WindowWithAdyen).AdyenCheckout;
      if (!adyenCheckoutConstructor) {
        throw new Error("AdyenCheckout API is niet beschikbaar.");
      }

      const checkout = await adyenCheckoutConstructor({
        environment,
        clientKey: this.props.clientKey,
        countryCode: this.props.countryCode ?? "NL",
        locale: this.props.locale ?? "nl-NL",
        showPayButton: this.props.showPayButton ?? true,
        amount: this.props.amount,
        session: this.props.session,
        onPaymentCompleted: (event: unknown) => {
          this.clearTimeout();
          this.props.onCompleted?.(event);
        },
        onPaymentFailed: (event: unknown) => {
          this.clearTimeout();
          this.props.onFailed?.(event);
        },
        onError: (event: unknown) => {
          this.clearTimeout();
          this.props.onError?.(event);
          this.setState({ error: "Er is een fout opgetreden in Adyen Checkout.", isLoading: false });
        }
      });

      this.dropin = checkout.create("dropin", {
        amount: this.props.amount,
        reference: this.props.reference,
        onSubmit: (event: unknown) => {
          this.props.onPending?.(event);
        }
      });

      if (!this.mountRef.current) {
        throw new Error("Adyen mount target ontbreekt.");
      }

      this.dropin.mount(this.mountRef.current);

      const timeoutMs = this.props.timeoutMs ?? 15 * 60 * 1000;
      this.timeoutHandle = window.setTimeout(() => {
        this.props.onTimeout?.({ timeoutMs, reference: this.props.reference });
      }, timeoutMs);

      this.setState({ isLoading: false, error: undefined });
    } catch (error) {
      this.clearTimeout();
      const message = error instanceof Error ? error.message : "Kon Adyen checkout niet initialiseren.";
      this.setState({ isLoading: false, error: message });
      this.props.onError?.(error);
    }
  }

  private clearTimeout(): void {
    if (this.timeoutHandle !== undefined) {
      window.clearTimeout(this.timeoutHandle);
      this.timeoutHandle = undefined;
    }
  }

  render(): React.ReactNode {
    return (
      <div className={this.props.className}>
        {this.state.isLoading ? <div>Adyen betaalcomponent wordt geladen…</div> : null}
        {this.state.error ? <div>{this.state.error}</div> : null}
        <div ref={this.mountRef} />
      </div>
    );
  }
}
